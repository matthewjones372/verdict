package verdict.macros

import verdict.Rule

/** Writes a rule as ordinary Scala, and gets the same AST the JSON loader and
  * the hand-written constructors produce.
  *
  * {{{
  * val r: Rule = rule[Trade] { t =>
  *   t.notional > 1000000 && (Set("DE", "FR", "IT").contains(t.counterparty.jurisdiction) || !t.isCleared)
  * }
  * }}}
  *
  * The lambda is never run. It is read at compile time and thrown away, which
  * is why only a small subset of Scala is accepted inside it.
  */
inline def rule[A](inline predicate: A => Boolean): Rule = ${ RuleMacro.build[A]('predicate) }
