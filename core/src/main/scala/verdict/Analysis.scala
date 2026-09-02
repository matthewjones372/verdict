package verdict

object Analysis:

  /** Every path the rule reads.
    *
    * This is only answerable because the AST spells out its own field access.
    * A rule holding a `Trade => Boolean` could not be asked.
    */
  def fieldsUsed(rule: Rule): Set[FieldPath] = rule match
    case Rule.And(l, r) => fieldsUsed(l) ++ fieldsUsed(r)
    case Rule.Or(l, r)  => fieldsUsed(l) ++ fieldsUsed(r)
    case Rule.Not(i)    => fieldsUsed(i)
    case Rule.Gt(p, _)  => Set(p)
    case Rule.Gte(p, _) => Set(p)
    case Rule.Lt(p, _)  => Set(p)
    case Rule.Lte(p, _) => Set(p)
    case Rule.EqStr(p, _) => Set(p)
    case Rule.In(p, _)    => Set(p)
    case Rule.IsTrue(p)   => Set(p)

  def describe(rule: Rule): String = rule match
    case Rule.Gt(p, v)  => s"${p.render} is greater than ${number(v)}"
    case Rule.Gte(p, v) => s"${p.render} is at least ${number(v)}"
    case Rule.Lt(p, v)  => s"${p.render} is less than ${number(v)}"
    case Rule.Lte(p, v) => s"${p.render} is at most ${number(v)}"
    case Rule.EqStr(p, v) => s"""${p.render} is "$v""""
    case Rule.IsTrue(p)   => s"${p.render} is true"
    case Rule.In(p, values) =>
      if values.isEmpty then s"${p.render} is one of nothing"
      else s"""${p.render} is one of ${values.toList.sorted.map(v => s""""$v"""").mkString(", ")}"""
    case Rule.Not(inner) => s"not (${describe(inner)})"
    case Rule.And(_, _)  => conjuncts(rule).map(bracket(_, isConjunction)).mkString(" and ")
    case Rule.Or(_, _)   => disjuncts(rule).map(bracket(_, isDisjunction)).mkString(" or ")

  /** Rewrites a rule into a smaller one with the same verdict.
    *
    * "Same verdict" is the claim, and it holds for rules that validate against
    * the schema they are evaluated with. It is deliberately weaker than "same
    * evidence": merging `x > 1 and x > 2` into `x > 2` removes a line from the
    * explanation, and dropping a duplicate conjunct removes another. The
    * property test in the tests module checks the verdict, not the tree.
    *
    * It is also weaker than "same errors": absorption can delete a branch that
    * would have failed to evaluate. A rule that validates has no such branch.
    */
  def simplify(rule: Rule): Rule = rule match
    case Rule.Not(inner) =>
      simplify(inner) match
        case Rule.Not(twice) => twice
        case simplified      => Rule.Not(simplified)

    case Rule.And(_, _) =>
      val parts = conjuncts(rule).map(simplify).flatMap(conjuncts)
      rebuild(absorb(collapse(parts, conjunction = true), conjunction = true), conjunction = true)

    case Rule.Or(_, _) =>
      val parts = disjuncts(rule).map(simplify).flatMap(disjuncts)
      rebuild(absorb(collapse(parts, conjunction = false), conjunction = false), conjunction = false)

    case leaf => leaf

  private def number(value: BigDecimal): String = FieldValue.Num(value).render

  private def isConjunction(rule: Rule): Boolean = rule match
    case Rule.And(_, _) => true
    case _              => false

  private def isDisjunction(rule: Rule): Boolean = rule match
    case Rule.Or(_, _) => true
    case _             => false

  /** Only the other connective needs brackets; `a and b and c` reads fine flat. */
  private def bracket(rule: Rule, sameConnective: Rule => Boolean): String =
    rule match
      case Rule.And(_, _) | Rule.Or(_, _) if !sameConnective(rule) => s"(${describe(rule)})"
      case _                                                       => describe(rule)

  private def conjuncts(rule: Rule): List[Rule] = rule match
    case Rule.And(l, r) => conjuncts(l) ++ conjuncts(r)
    case other          => List(other)

  private def disjuncts(rule: Rule): List[Rule] = rule match
    case Rule.Or(l, r) => disjuncts(l) ++ disjuncts(r)
    case other         => List(other)

  private def rebuild(parts: List[Rule], conjunction: Boolean): Rule =
    parts.reduceRight((l, r) => if conjunction then Rule.And(l, r) else Rule.Or(l, r))

  /** Folds each part into the first earlier part it can merge with, so the
    * surviving order is the order the rule was written in.
    */
  private def collapse(parts: List[Rule], conjunction: Boolean): List[Rule] =
    parts.foldLeft(List.empty[Rule]) { (kept, next) =>
      kept.indexWhere(existing => merge(existing, next, conjunction).isDefined) match
        case -1    => kept :+ next
        case index => kept.updated(index, merge(kept(index), next, conjunction).get)
    }

  /** `a and (a or b)` is `a`, and `a or (a and b)` is `a`. */
  private def absorb(parts: List[Rule], conjunction: Boolean): List[Rule] =
    val absorbed = parts.filterNot { part =>
      val inner = if conjunction then disjuncts(part) else conjuncts(part)
      inner.length > 1 && inner.exists(branch => parts.exists(_ == branch))
    }
    if absorbed.isEmpty then parts else absorbed

  /** Two constraints on the same path, combined into one where the language can
    * say the result. Constraints on different paths never merge: nothing here
    * knows that a record has one value per path — only that the same path reads
    * the same way twice within one record, which is what makes this sound.
    */
  private def merge(left: Rule, right: Rule, conjunction: Boolean): Option[Rule] =
    if left == right then Some(left)
    else
      (textSet(left), textSet(right)) match
        case (Some((p, a)), Some((q, b))) if p == q =>
          Some(fromSet(p, if conjunction then a.intersect(b) else a.union(b)))
        case _ =>
          (lowerBound(left), lowerBound(right)) match
            case (Some((p, a)), Some((q, b))) if p == q =>
              Some(fromLower(p, combine(a, b, keepLarger = conjunction, conjunction)))
            case _ =>
              (upperBound(left), upperBound(right)) match
                case (Some((p, a)), Some((q, b))) if p == q =>
                  Some(fromUpper(p, combine(a, b, keepLarger = !conjunction, conjunction)))
                case _ => None

  /** A bound is a value and whether the value itself is included. */
  private type Bound = (BigDecimal, Boolean)

  /** `keepLarger` says which end tightens for this kind of bound: a lower bound
    * tightens upwards, an upper bound downwards, and a disjunction wants the
    * loose end of whichever it is. On equal values the two bounds differ only
    * in whether the value itself counts, and there a conjunction takes the
    * exclusive one and a disjunction the inclusive one.
    */
  private def combine(a: Bound, b: Bound, keepLarger: Boolean, conjunction: Boolean): Bound =
    if a._1 == b._1 then (a._1, if conjunction then a._2 && b._2 else a._2 || b._2)
    else if (a._1 > b._1) == keepLarger then a
    else b

  private def textSet(rule: Rule): Option[(FieldPath, Set[String])] = rule match
    case Rule.EqStr(p, v) => Some(p -> Set(v))
    case Rule.In(p, vs)   => Some(p -> vs)
    case _                => None

  private def fromSet(path: FieldPath, values: Set[String]): Rule =
    if values.sizeIs == 1 then Rule.EqStr(path, values.head) else Rule.In(path, values)

  private def lowerBound(rule: Rule): Option[(FieldPath, Bound)] = rule match
    case Rule.Gt(p, v)  => Some(p -> (v, false))
    case Rule.Gte(p, v) => Some(p -> (v, true))
    case _              => None

  private def fromLower(path: FieldPath, bound: Bound): Rule =
    if bound._2 then Rule.Gte(path, bound._1) else Rule.Gt(path, bound._1)

  private def upperBound(rule: Rule): Option[(FieldPath, Bound)] = rule match
    case Rule.Lt(p, v)  => Some(p -> (v, false))
    case Rule.Lte(p, v) => Some(p -> (v, true))
    case _              => None

  private def fromUpper(path: FieldPath, bound: Bound): Rule =
    if bound._2 then Rule.Lte(path, bound._1) else Rule.Lt(path, bound._1)
