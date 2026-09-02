package verdict.macros

import verdict.{FieldTag, Identity, Schema, Sensitive}

import scala.deriving.Mirror
import scala.quoted.*

/** Schema derivation that also reads field annotations.
  *
  * `Mirror` exposes labels and types but not annotations, so the tags have to
  * come from `quotes.reflect`. A macro cannot be expanded in the compilation
  * unit that defines it, which is why this lives outside core rather than
  * inside `Schema.derived`.
  */
object Annotated:

  inline def schema[A](using Mirror.ProductOf[A]): Schema[A] =
    Schema.derived[A].tagged(fieldTags[A])

  /** Field tags keyed by dotted path, so that nested records are covered too. */
  inline def fieldTags[A]: Map[String, Set[FieldTag]] = ${ fieldTagsImpl[A] }

  private def fieldTagsImpl[A: Type](using Quotes): Expr[Map[String, Set[FieldTag]]] =
    import quotes.reflect.*

    def isRecord(tpe: TypeRepr): Boolean =
      val sym = tpe.typeSymbol
      sym.flags.is(Flags.Case) && sym.caseFields.nonEmpty

    def tagsOf(accessor: Symbol, constructorParams: Map[String, Symbol]): List[Expr[FieldTag]] =
      // The trap: on a case class, `@Sensitive val x` is written in the
      // parameter list, and the annotation lands on the *constructor parameter*
      // symbol. The generated field accessor that `caseFields` returns does not
      // carry it, so looking only there finds nothing at all. Both symbols are
      // consulted because a `val` declared in the body annotates the accessor.
      val symbols = accessor :: constructorParams.get(accessor.name).toList
      val found = symbols.flatMap(_.annotations).flatMap { annotation =>
        if annotation.tpe <:< TypeRepr.of[Sensitive] then Some("Sensitive")
        else if annotation.tpe <:< TypeRepr.of[Identity] then Some("Identity")
        else None
      }.distinct.sorted
      found.map {
        case "Identity" => '{ FieldTag.Identity }
        case _          => '{ FieldTag.Sensitive }
      }

    def collect(tpe: TypeRepr, prefix: String, unfolding: Set[Symbol]): List[Expr[(String, Set[FieldTag])]] =
      val sym = tpe.typeSymbol
      if unfolding.contains(sym) then Nil
      else
        val constructorParams =
          val ctor = sym.primaryConstructor
          if ctor.isNoSymbol then Map.empty[String, Symbol]
          else ctor.paramSymss.flatten.filterNot(_.isTypeParam).map(p => p.name -> p).toMap

        sym.caseFields.flatMap { accessor =>
          val path      = if prefix.isEmpty then accessor.name else s"$prefix.${accessor.name}"
          val fieldType = tpe.memberType(accessor)
          val here      = tagsOf(accessor, constructorParams)
          val mine =
            if here.isEmpty then Nil
            else List('{ (${ Expr(path) }, ${ Expr.ofList(here) }.toSet) })
          val below = if isRecord(fieldType) then collect(fieldType, path, unfolding + sym) else Nil
          mine ++ below
        }

    val entries = collect(TypeRepr.of[A], "", Set.empty)
    '{ ${ Expr.ofList(entries) }.toMap }
