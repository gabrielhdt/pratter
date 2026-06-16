# Changelog

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
Lines marked with 🧨 describe breaking changes.

## [Unreleased]

## [5.0.1] -- 2025-02-27

Internal edits to reduce the minimal supported ocaml version to 4.10.0.

## [5.0] -- 2025-02-12

### Changed

🧨 The Signature of `expression` has changed from
```ocaml
appl:('b -> 'b -> 'b) -> token:('a -> 'b)
-> ops:(('a, 'b) Operators.t)
-> ('a, 'b) parser
```
to
```ocaml
appl:('b -> 'b -> 'b) -> token:('a -> 'b)
-> ops:('a -> (fixity * float * 'b) list)
-> ('a, 'b) parser
```

### Removed

The `Operators` module has been deleted.

## [4.0] -- 2024-10-08

### Changed

The parser is made functional: the use of `camlp-streams` has been dropped in
favour of sequences of the `Seq` module. The parser is written in a monad.

🧨 The error `` `Op_conflict`` now takes a single argument instead of two.

🧨 Introduced a type for parsers. The function `expression` builds a parser
that can then be run on any sequence with the `run` function.

🧨 A token can have different fixities depending on the context its in. For
instance, the `-` can be declared both prefix and infix, resulting in `- x - y`
being parsed as `(-x) - y`. Consequently the API changed. The parser now truly
parses the input, meaning that the output datatype may be different from the
input datatype: the main `expression` function changed from
- `expression : ... -> 'a Stream.t -> 'a` to
- `expression : ... -> 'a Stream.t -> 'b`.

Consequently, the `~appl` argument of the expression function now operates on
the output type, and a new argument `~token` must be given which parses tokens
that aren't operators.

The expression function also takes an `Operators.t` value instead of a function
`is_op: 'a -> (fixity * float) option` to parse operators. To use the new datatype,
the rule is the following. For any operator `op`
- if `is_op op` returns `Some (Infix a, p)`, the operators parser must be built
  with `infix f a p` where `f` is a function that parses `o` and produces an
  output datatype.
- if `is_op op` returns `Some (Prefix, p)`, then the operators parser must be
  built with `prefix f p`, where `f` verifies the aforementioned properties.
- if `is_op op`  returns `Some (Postfix, p)`, then the operators parser must be
  built with `postfix f p`, and `f` verifies the aforementioned properties.

🧨 Use camel case for errors: `OpConflict` changed to `Op_conflict` and
`TooFewArguments` to `Too_few_arguments`.

### Removed

🧨 Errors `UnexpectedPostfix` and `UnexpectedInfix` have been removed. If a
infix operator appears postfix, it's handled by the token parser rather than
the operator parser.

## [3.0.1] -- 2023-11-06

### Added

Lower bounds on test dependencies `alcotest`, `qcheck` and `qcheck-alcotest`.

## [3.0] -- 2023-11-04

### Changed

🧨 Removed the functor interface, the parser is a single function taking two
functions and a stream of terms as parameters.

No more `table` type needed, the parser only needs a function `is_op` to
distinguish operators from other tokens.

To port code, any snippet like
```ocaml
module S : SUPPORT = struct
  type term = ...
  type table = ...
  let get tbl t = ...
  let make_appl t u = ...
end
module P = Pratter.Make(S)
```
should be replaced by
```ocaml
let is_op t = ...
let appl t u = ...
```
where the following properties hold
- `appl = S.make_appl`
- `∃ tbl : S.table, get tbl = is_op`

Under these assumptions,
denoting `tbl` the value that makes the second hypothesis hold,
we have `P.expression tbl = Pratter.expression ~is_op ~appl`

## [2.0] -- 2022-06-15

### Added

- Postfix operators
- Comparison against a YACC parser for a small arithmetic-like language of the form
  `t ::= t t | t + t | - t | t * t | t = t | t !`

### Changed

- 🧨 `Una` constructor changed to `Prefix`
- 🧨 `Bin` constructor changed to `Infix`
- 🧨 Errors are encoded with a polymorphic variant rather than exceptions.
  To port code, replace sections of the form
  ```ocaml
  try let t = SupPrat.expression tbl s in e with
  | ... -> ...
  ```
  with
  ```ocaml
  match SupPrat.expression tbl s with
  | Ok t -> e
  | Error ... -> ...
  ```

## [1.2.1] -- 2022-05-06

### Added

- Property based testing with QCheck.

### Changed

- Depends on `camlp-streams` (because `Stdlib.Stream` becomes deprecated in OCaml 4.14).
- New continuous integration recipe.

## [1.2] -- 2021-05-05

### Added

- Walkthrough in README.md.

### Fixed

- Parsing a binary operator without left context fails.
- Binding power can be negative.

## [1.1] -- 2021-01-23

### Added

- Non associative operators
- Error handling on partially applied operators (which raises a
  `Stream.Failure`)

### Changed

- One function `get` for operators in API
- `make_appl` does not use the table of operators

## [1.0.1] -- 2021-01-16

### Fixed

- Correct OCaml dependency
- Tests comply with OCaml 4.02

### Added

- Gitlab continuous integration

## [1.0] -- 2021-01-14

### Changed

- API: parser uses a data structure passed as argument
- renamed CHANGELOG to CHANGELOG.md

## [0.1.1] -- 2021-01-06

### Added

- Initial version
