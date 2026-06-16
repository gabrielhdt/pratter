# [Pratter](https://forge.tedomum.net/koizel/pratter)

By Gabriel Hondet

Pratter helps you parsing streams of tokens interspersed with prefix, postfix
or infix operators.
Unlike parser generators such as
[ocamlyacc](https://v2.ocaml.org/manual/lexyacc.html) or
[Menhir](http://gallium.inria.fr/~fpottier/menhir/),
parsing rules are written in OCaml and they can be edited dynamically.
On the other hand Pratter can parse
a very limited subset of what Menhir and yacc can.

You are free to copy, modify and distribute Pratter with attribution under the
terms of the BSD 3 Clause license. See the [license](./LICENSE) for more details.

## Getting started

To compile and use pratter, you need

- ocaml >= 4.08
- dune >= 2.7

Then, at the root of the source tree,
```command
$ dune build
$ dune install
```

You can try the library in the toplevel: the following code defines a parser
for the language made of strings interspersed with infix `+` operators:
```ocaml
# #require "pratter";;
# type t = A of t * t | S of string;;
type t = A of t * t | S of string
# let appl t u = A (t, u);;
val appl : t -> t -> t = <fun>
# let is_op = function S "+" -> Some Pratter.(Infix Left, 0.3) | _ -> None;;
val is_op : t -> (Pratter.operator * float) option = <fun>
# Pratter.expression ~appl ~is_op (Stream.of_list [ S "x"; S "+"; S "y"]);;
- : (t, t Pratter.error) result = Ok (A (A (S "+", S "x"), S "y"))
```

## What next?

- There's a [change log](./CHANGELOG.md).
- There's a [license](./LICENSE).
- [This file](./t/simple.ml) is another use case example that is slightly more
  involved than the one in this readme.

You can raise issues either using the [issue
tracker](https://forge.tedomum.net/koizel/pratter/issues)
or sending an email to `<koizel#pratter@aleeas.com>`.
