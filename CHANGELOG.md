# Changelog
This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
Lines marked with 🧨 describe breaking changes.

## [Unreleased]

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
