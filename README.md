# verdict

A rules engine for Scala 3 where a rule is a value rather than a compiled
predicate. The same rule can be evaluated, explained, compiled to SQL, checked
against a record type, and asked what fields it reads.

The AST has no case holding a function. A node holding a `Trade => Boolean`
would hide that part of the rule from validation, from the SQL interpreter, and
from anyone reading it, so everything else depends on that restriction.

## The two authoring routes

A rule can be written as Scala and read at compile time:

```scala
import verdict.macros.rule

val r: Rule = rule[Trade] { t =>
  t.notional > 1000000 && (Set("DE", "FR", "IT").contains(t.counterparty.jurisdiction) || !t.isCleared)
}
```

The lambda is never run. A macro reads it, checks each field path against the
record type, and produces the AST. The supported subset is `&&`, `||`, `!`,
comparisons against a numeric literal, `==` against a string literal,
`Set(...).contains(field)`, and a boolean field on its own. Anything else is a
compile error that names the expression and lists what is supported.

Or it can be loaded from JSON:

```json
{"op":"and",
 "left":  {"op":"gt","path":"notional","value":1000000},
 "right": {"op":"or",
           "left":  {"op":"in","path":"counterparty.jurisdiction","values":["DE","FR","IT"]},
           "right": {"op":"not","rule":{"op":"isTrue","path":"isCleared"}}}}
```

```scala
RuleJson.load(document, Schema.of[Trade]): Either[List[ValidationError], Rule]
```

Both produce the same `Rule`. JSON is for storage, not for writing rules by
hand. Rules are written with the macro, or through a UI built over the derived
schema, which already has the field names, their types and their tags.

## A worked example

`sbt "tests/runMain verdict.demo.Demo"`, pasted from a run:

```
=== Schema, derived from the case class ===
Trade
  id: text @Identity
  notional: number
  currency: text
  isCleared: boolean
  counterparty: Counterparty
    name: text @Sensitive
    jurisdiction: text
    isSanctioned: boolean
    rating: number

=== Rule, written as Scala and read at compile time ===
notional is greater than 1000000 and (counterparty.jurisdiction is one of "DE", "FR", "IT" or not (isCleared is true))

=== The same rule, loaded from storage ===
  loaded:            notional is greater than 1000000 and (counterparty.jurisdiction is one of "DE", "FR", "IT" or not (isCleared is true))
  same as authored:  true

=== What the rule reads ===
  counterparty.jurisdiction, isCleared, notional

=== Compiled to SQL ===
  SELECT * FROM trades WHERE ((notional > ?) AND ((counterparty_jurisdiction IN (?, ?, ?)) OR NOT ((is_cleared = ?))))
  binds: 1000000, DE, FR, IT, true

=== Evidence ===
T-1 matches
  PASS all of:
    PASS notional (2500000) > 1000000
    PASS any of:
      PASS counterparty.jurisdiction (DE) in {DE, FR, IT}
      FAIL not:
        PASS isCleared (true) is true

T-2 does not match
  FAIL all of:
    FAIL notional (125.5) > 1000000
    PASS any of:
      FAIL counterparty.jurisdiction (US) in {DE, FR, IT}
      PASS not:
        FAIL isCleared (false) is true

T-3 matches
  PASS all of:
    PASS notional (9000000) > 1000000
    PASS any of:
      FAIL counterparty.jurisdiction (RU) in {DE, FR, IT}
      PASS not:
        FAIL isCleared (false) is true
```

## How it works

`Schema.derived` reads the case class through `Mirror.ProductOf`, so nobody
maintains a field list. `@Sensitive` and `@Identity` are read by a separate
macro, because `Mirror` does not carry annotations. Rename a field and the rules
written against the old name stop validating; that is what the schema is for.

`evaluate` returns an `Evidence` tree and `holds` reads the verdict off it.
There is no separate fast path to fall out of step with the explanation.

The in-memory evaluator and the generated SQL are two implementations of the
same rule, so they are checked against each other: random trades go into H2, and
for a set of rules both sides must select exactly the same records. Half the
generated notionals are drawn from the constants the rules compare against,
because a purely random decimal almost never lands exactly on a boundary. With
those values, a `>` compiled as `>=` fails the test and shrinks to a single trade
sitting on the boundary.

## When not to use this

If you have one filter in one service, write the `if`. This library costs you an
AST, a schema derivation, a validation pass and a macro, and buys you
explainability and multiple interpreters. That trade is a loss when:

- the rule lives in one place and is read by the people who wrote it;
- engineers are the only audience, so "why did this match?" is answered by
  reading the code or a log line;
- nothing outside the service needs the same rule, so there is no second
  interpreter;
- the rules never change without a deploy, so storing them buys nothing.

The cost is worth paying when a non-engineer has to see why a record matched,
when the same rule must run in the application and in the database, or when
rules change on a different schedule from the code.

## Prior art

The `Evidence` tree and the `rule` macro use the same idea as ZIO Test's
`assertTrue`: an assertion written as ordinary Scala is intercepted by a
macro and reified into composable `TestArrow` values, which is why a failure can
report the parts of the expression rather than just `false`. This library
applies that idea to business rules instead of test assertions.

## Layout

- `core`: the AST, the schema types and every interpreter. No macros, no
  dependencies.
- `macros`: the annotation reader and the `rule[A] { ... }` macro. A macro
  cannot be expanded in the compilation unit that defines it, so this is its own
  subproject.
- `tests`: the specs, the differential test, and the demo main.

`sbt test` runs everything.
