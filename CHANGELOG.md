# Changelog
This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
Lines marked with 🧨 describe breaking changes.

## [Unreleased]
### Added
- Postfix operators
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
