# [Pratter](https://forge.tedomum.net/koizel/pratter)

By Gabriel Hondet

Pratter is a library that allows to parse expressions with infix, postfix or
prefix operators.

Pratter is a _top-down operator precedence parser_ that implements the [Pratt
parsing algorithm](https://web.archive.org/web/20151223215421/http://hall.org.ua/halls/wizzard/pdf/Vaughan.Pratt.TDOP.pdf).
In contrast with [Menhir](https://gallium.inria.fr/~fpottier/menhir/)
or ocamlyacc, Pratter is a parser, not a parser generator:
no code is produced during compilation.
In consequence, parsing rules can be modified
dynamically. On the other hand, Pratter parses
a much more restricted class of grammars called
[_operator grammars_](https://en.wikipedia.org/wiki/Operator-precedence_grammar)
while Menhir and ocamlyacc parse LR(1) and LALR(1) grammars respectively.

You are free to copy, modify and distribute Pratter with attribution under the
terms of the BSD 3 Clause license. See the [license](./LICENSE) for more details.

## Getting started

To compile and use pratter, you need

- ocaml >= 4.10
- dune >= 2.7

Then, at the root of the source tree,
```command
$ make install
```

To ensure it's working write the following code in some file `plus.ml`
to parse the language defined by the grammar `T ::= T + T | id`
where `id` denotes identifiers
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

## What next?

- There's a [change log](./CHANGELOG.md).
- There's a [license](./LICENSE).
- [This file](./t/simple.ml) is another use case example that is slightly more
  involved than the one in this readme.

You can report issues, ask questions or start discussions using the
[issue tracker](https://forge.tedomum.net/koizel/pratter/issues).
