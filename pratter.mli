(* Copyright (C) Gabriel Hondet.
   Subject to the BSD-3-Clause license *)

(** Parse expressions with infix, prefix or postfix operators.

    To parse expressions from type ['i] to type ['o], you need to tell the
    parser
    - how to concatenate two expressions with a function of type
      ['o -> 'o -> 'o];
    - how to determine whether a value of ['i] should be considered an operator;
    - how to parse tokens of ['i] that aren't operators.

    The algorithm implemented is an extension of the Pratt parser. The Shunting
    Yard algorithm could also be used.
    @see <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
    @see <https://dev.to/jrop/pratt-parsing> *)

(** Associativity of an operator. *)
type associativity =
  | Left
      (** If [+] is a left associative operator, [x + y + z] is parsed
          [(x + y) + z]. *)
  | Right
      (** If [+] is a right associative operator, [x + y + z] is parsed
          [x + (y + z)]. *)
  | Neither
      (** If [+] is not associative, then [(x + y) + z] is not [x + (y + z)] and
          [x + y + z] results in a syntax error. *)

(** The fixity of an operator *)
type fixity =
  | Infix of associativity  (** Infix operator like [+] in [x + y]. *)
  | Prefix  (** Prefix operator like [!] in [! x]. *)
  | Postfix  (** Postfix operator like [^] in [x ^]. *)

type 't error =
  [ `Op_conflict of 't
    (** Priority or associativiy conflict between two operators. In
        [`OpConflict o], [o] is an operator which generates a conflict. *)
  | `Too_few_arguments
    (** More arguments are expected. It is raised for instance on partial
        application of operators, such as [x +]; or when an empty input is given
        to the parser. *) ]
(** Errors that can be encountered while parsing a stream of terms. *)

type ('a, 'b) result = ('a, 'b error) Stdlib.result
(** A specialised error type. *)

type ('tok, 'out) parser
(** Parsers from sequences of ['tok] to values of type ['out]. *)

val expression :
     appl:('b -> 'b -> 'b)
  -> token:('a -> 'b)
  -> ops:('a -> (fixity * float * 'b) list)
  -> ('a, 'b) parser
(** [expression appl token ops] is a parser from sequences of ['a] to values of
    type ['b]. The parser is driven by the operator table [ops] which determines
    which tokens are operators. A token [t] can be used as an operator if
    [ops t] isn't empty. Each value [(f, p, s)] of [ops t] means that token [t]
    can be parsed as token [s] with fixity [f] and priority [p].

    The function [appl] is used to combine two expressions.

    For instance, assuming that [+] is declared infix and we're working with
    numbers, it can transform [3 + 5 × 2] encoded as the stream of terms
    [3, +, 5, ×, 2] into the binary tree [@(@(×,@(@(+,3),5)),2)] where [@]
    denotes application nodes. Such a parsing could be produced by the
    parameters [ops] and [appl] such that (types of tokens are schematised for
    simplicity)
    - [ops + = [Infix Left, 1.0, +]] and
    - [appl x y = @(x, y)]. *)

val run : ('tok, 'out) parser -> 'tok Seq.t -> ('out, 'tok) result
(** Run the given parser on a sequence of tokens. *)
