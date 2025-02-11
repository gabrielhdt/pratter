# [Pratter](https://forge.tedomum.net/koizel/pratter)

By Gabriel Hondet

Pratter allows to transform strings of symbols and mixfix operators to full
binary trees.
Pratter is convenient for parsing languages made of terms with many mixfix
operators with different associativities and precedences such as
arithmetic or λ-calculi.
In contrast to parser generators, parsing rules can be edited dynamically.

You are free to copy, modify and distribute Pratter with attribution under the
terms of the BSD 3 Clause license. See the [license](./LICENSE) for more details.

## Getting started

To compile and use pratter, you need

- ocaml >= 4.14
- dune >= 2.7

Then, at the root of the source tree,
```command
$ make install
```

To ensure it's working write the following code in some file `plus.ml`
```ocaml
type t = A of t * t | S of string
let appl t u = A (t, u)
let token = Fun.id
let ops = function
  | S "+" as s -> Pratter.[ Infix Left, 0.3, s ]
  | _ -> []
let parse = Pratter.expression ~token ~appl ~ops
let () =
  let input = List.to_seq [ S "x"; S "+"; S "y"] in
  assert (Result.is_ok @@ Pratter.run parse input)
```
then execute the following lines which should return 0
```command
$ echo "module Pratter = struct $(cat pratter.ml) end $(cat plus.ml)" > plus_.ml
$ ocamlc plus_.ml
$ ./a.out
```

The aforementioned code defines a parser for the language made of strings
interspersed with infix `+` operators

## What next?

- There's a [change log](./CHANGELOG.md).
- There's a [license](./LICENSE).
- [This file](./t/simple.ml) is another use case example that is slightly more
  involved than the one in this readme.

You can raise issues either using the [issue
tracker](https://forge.tedomum.net/koizel/pratter/issues)
or sending an email to `<koizel#pratter@aleeas.com>`.
