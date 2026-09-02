package verdict.macros

import verdict.{FieldPath, Rule}

import scala.quoted.*

object RuleMacro:

  def build[A: Type](predicate: Expr[A => Boolean])(using Quotes): Expr[Rule] =
    import quotes.reflect.*

    val supported =
      "&&, ||, !, the comparisons > >= < <= against a numeric literal, == against a string literal, " +
        "Set(...).contains(field), and a boolean field on its own"

    def fail(term: Term, why: String): Nothing =
      // The subset is the feature. A rule that could hold any Scala expression
      // would be a compiled predicate again, opaque to validation, to SQL and
      // to every reader; refusing here is what keeps the AST able to describe
      // itself. So the message names the expression and says what can be said.
      report.errorAndAbort(s"verdict: cannot read `${term.show}` as a rule — $why. Supported: $supported.", term.pos)

    def strip(term: Term): Term = term match
      case Inlined(_, _, inner) => strip(inner)
      case Block(Nil, inner)    => strip(inner)
      case Typed(inner, _)      => strip(inner)
      case other                => other

    def lambda(term: Term): (Symbol, Term) = strip(term) match
      case Block(List(definition: DefDef), _: Closure) =>
        val parameter = definition.termParamss.headOption.flatMap(_.params.headOption)
        (parameter, definition.rhs) match
          case (Some(param), Some(body)) => (param.symbol, body)
          case _                         => fail(term, "the lambda has no parameter to read fields from")
      case other => fail(other, "this is not a lambda written out at the call site")

    /** `t.counterparty.jurisdiction` is a chain of Selects ending at the
      * lambda's own parameter; anything else is not a field of this record.
      */
    def pathOf(term: Term, param: Symbol): Option[List[String]] = strip(term) match
      case identifier: Ident if identifier.symbol == param => Some(Nil)
      case Select(qualifier, name)                         => pathOf(qualifier, param).map(_ :+ name)
      case _                                               => None

    def isConversion(fn: Term): Boolean =
      val symbol = fn.symbol
      symbol.flags.is(Flags.Implicit) ||
      symbol.flags.is(Flags.Given) ||
      (symbol.name == "apply" && symbol.owner.name.startsWith("BigDecimal"))

    def numberOf(term: Term): Option[BigDecimal] = strip(term) match
      case Literal(IntConstant(v))    => Some(BigDecimal(v))
      case Literal(LongConstant(v))   => Some(BigDecimal(v))
      case Literal(ShortConstant(v))  => Some(BigDecimal(v.toInt))
      case Literal(ByteConstant(v))   => Some(BigDecimal(v.toInt))
      case Literal(DoubleConstant(v)) => Some(BigDecimal(v.toString))
      case Literal(FloatConstant(v))  => Some(BigDecimal(v.toString))
      case Select(inner, "unary_-")   => numberOf(inner).map(-_)
      // A numeric literal reaches a BigDecimal comparison through a conversion.
      // Only conversions are unwrapped: unwrapping any single-argument call
      // would read `f(5)` as the number five.
      case Apply(fn, List(argument)) if isConversion(fn)        => numberOf(argument)
      case Apply(Apply(fn, List(argument)), _) if isConversion(fn) => numberOf(argument)
      case TypeApply(inner, _)                                  => numberOf(inner)
      case _                                                    => None

    def stringOf(term: Term): Option[String] = strip(term) match
      case Literal(StringConstant(v)) => Some(v)
      case _                          => None

    def setOf(term: Term): Option[List[String]] = strip(term) match
      case Apply(TypeApply(Select(qualifier, "apply"), _), List(elements))
          if qualifier.symbol.name == "Set" =>
        strip(elements) match
          case Repeated(values, _) =>
            val strings = values.flatMap(stringOf)
            Option.when(strings.length == values.length)(strings)
          case _ => None
      case _ => None

    def leafKind(tpe: TypeRepr): Option[String] =
      if tpe =:= TypeRepr.of[String] then Some("text")
      else if tpe =:= TypeRepr.of[BigDecimal] || tpe =:= TypeRepr.of[Int] || tpe =:= TypeRepr.of[Long] then Some("number")
      else if tpe =:= TypeRepr.of[Boolean] then Some("boolean")
      else None

    def resolve(tpe: TypeRepr, segments: List[String]): Either[String, String] =
      segments match
        case Nil =>
          leafKind(tpe).toRight(s"${tpe.typeSymbol.name} is a record, and a rule compares values")
        case name :: rest =>
          tpe.typeSymbol.caseFields.find(_.name == name) match
            case None =>
              val available = tpe.typeSymbol.caseFields.map(_.name)
              Left(s"there is no field '$name' on ${tpe.typeSymbol.name}; the fields are ${available.mkString(", ")}")
            case Some(field) => resolve(tpe.memberType(field), rest)

    /** The typo becomes a compile error here rather than a validation failure
      * at load time, because the record type is known at the call site.
      */
    def checked(term: Term, segments: List[String], expected: String): Expr[FieldPath] =
      resolve(TypeRepr.of[A], segments) match
        case Left(problem) => report.errorAndAbort(s"verdict: $problem.", term.pos)
        case Right(actual) if actual != expected =>
          report.errorAndAbort(
            s"verdict: ${segments.mkString(".")} is a $actual field, and this compares it as a $expected.",
            term.pos
          )
        case Right(_) =>
          val rendered = Expr(segments.mkString("."))
          '{ FieldPath.unsafe($rendered) }

    def field(term: Term, param: Symbol, expected: String): Expr[FieldPath] =
      pathOf(term, param) match
        case Some(Nil) | None => fail(term, "this is not a field of the record the rule is written against")
        case Some(segments)   => checked(term, segments, expected)

    def comparison(term: Term, param: Symbol, path: Term, value: Term, operator: String): Expr[Rule] =
      val column = field(path, param, "number")
      val bound  = numberOf(value).getOrElse(fail(value, "the right-hand side of a comparison must be a numeric literal"))
      val lifted = Expr(bound.toString)
      operator match
        case ">"  => '{ Rule.Gt($column, BigDecimal($lifted)) }
        case ">=" => '{ Rule.Gte($column, BigDecimal($lifted)) }
        case "<"  => '{ Rule.Lt($column, BigDecimal($lifted)) }
        case _    => '{ Rule.Lte($column, BigDecimal($lifted)) }

    def membership(collection: Term, element: Term, param: Symbol): Expr[Rule] =
      setOf(collection) match
        case Some(values) =>
          val column = field(element, param, "text")
          val lifted = Expr.ofList(values.map(Expr(_)))
          '{ Rule.In($column, $lifted.toSet) }
        case None => fail(collection, "contains is only understood on a literal Set of string literals")

    val flipped = Map(">" -> "<", ">=" -> "<=", "<" -> ">", "<=" -> ">=")

    def convert(term: Term, param: Symbol): Expr[Rule] = strip(term) match
      case Apply(Select(left, "&&"), List(right)) =>
        '{ Rule.And(${ convert(left, param) }, ${ convert(right, param) }) }

      case Apply(Select(left, "||"), List(right)) =>
        '{ Rule.Or(${ convert(left, param) }, ${ convert(right, param) }) }

      case Select(inner, "unary_!") =>
        '{ Rule.Not(${ convert(inner, param) }) }

      case Apply(Select(left, operator), List(right)) if flipped.contains(operator) =>
        if pathOf(left, param).exists(_.nonEmpty) then comparison(term, param, left, right, operator)
        else comparison(term, param, right, left, flipped(operator))

      case Apply(Select(left, "=="), List(right)) =>
        (stringOf(right), stringOf(left)) match
          case (Some(expected), _) => '{ Rule.EqStr(${ field(left, param, "text") }, ${ Expr(expected) }) }
          case (_, Some(expected)) => '{ Rule.EqStr(${ field(right, param, "text") }, ${ Expr(expected) }) }
          case _                   => fail(term, "== compares a field against a string literal")

      // `Set(...).contains` takes no type argument and `List(...).contains`
      // takes one, so both shapes are matched to reach the same message.
      case Apply(TypeApply(Select(collection, "contains"), _), List(element)) =>
        membership(collection, element, param)

      case Apply(Select(collection, "contains"), List(element)) =>
        membership(collection, element, param)

      case candidate if pathOf(candidate, param).exists(_.nonEmpty) =>
        '{ Rule.IsTrue(${ field(candidate, param, "boolean") }) }

      case other => fail(other, "this is not one of the shapes a rule can take")

    val (param, body) = lambda(predicate.asTerm)
    convert(body, param)
