(* Copyright (C) Gabriel Hondet.
   Subject to the BSD-3-Clause license *)

(** Transform strings of tokens and mixfix operators into full binary trees.
    Operators are characterised by their associativity and their fixity.

    To parse expressions from type ['i] to type ['o], you need to tell the
    parser
    - how to concatenate two expressions with a function of type
      ['o -> 'o -> 'o]; this function can be seen as the concatenation of two
      binary trees (and in that case, the input of the parser is a string of
      leaves);
    - how to determine whether a value of ['i] should be considered an
      operator;
    - how to parse tokens of ['i] that aren't operators.

    The algorithm implemented is an extension of the Pratt parser. The Shunting
    Yard algorithm could also be used.
    @see <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
    @see <https://dev.to/jrop/pratt-parsing> *)

(** Associativity of an operator. *)
type associativity =
  | Left
      (** If [+] is a left associative operator, [x + y + z] is parsed [(x +
          y) + z]. *)
  | Right
      (** If [+] is a right associative operator, [x + y + z] is parsed [x +
          (y + z)]. *)
  | Neither
      (** If [+] is not associative, then [(x + y) + z] is not [x + (y + z)] and
          [x + y + z] results in a syntax error. *)

type 't error =
  [ `Op_conflict of 't
    (** Priority or associativiy conflict between two operators.
        In [`OpConflict o], [o] is an operator which generates a conflict. *)
  | `Too_few_arguments
    (** More arguments are expected. It is raised for instance on
        partial application of operators, such as [x +]; or when an empty input
        is given to the parser. *)
  ]
(** Errors that can be encountered while parsing a stream of terms. *)

type ('a, 'b) result = ('a, 'b error) Stdlib.result
(** A specialised error type. *)

module Operators : sig
  type ('a, 'b) t
  (** The elements of that type drive the parser telling it which input tokens
      are operators. *)

  val none : _ t
  (** Tell the parser there's no operator. *)

  val infix : ('a -> 'b option) -> associativity -> float -> ('a, 'b) t
  (** [infix is a pr] tells the parser that for any input [i], if [is i] is
      true, then it's an infix operator with associativity [a] and precedence
      [pr].

      For example, use [infix = Neither 10.] to consider the equality [=] as
      infix with no associativity so that you can parse [x = y]. *)

  val prefix : ('a -> 'b option) -> float -> ('a, 'b) t
  (** [prefix is pr] tells the parser that for any input [i], if [is i] is true,
      then it's a prefix operator with precedence [pr].

      For example, use [prefix ¬ 1.] to consider the negation [¬] as a prefix
      operator so that you can parse [¬ x]. *)

  val postfix : ('a -> 'b option) -> float -> ('a, 'b) t
  (** [postfix is pr] tells the parser that for any input [i], if [is i] is true,
      then it's a postfix operator with precedence [pr].

      For example, use [postfix ! 1.] to consider the factorial [!] as a postfix
      operator so that you can parse [x !]. *)

  val ( <+> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** [o <+> p] tells the parser to consider the operators specified in [o] and
      in [p]. *)

  (** Note that through {!none} and {!(<+>)}, the type ['a t] is a {b semi-group}.
      You can use {!infix}, {!prefix} and {!postfix} to generate initial values
      and combine them with {!(<+>)}. *)

  val cat : ('a, 'b) t list -> ('a, 'b) t
  (** Concatenate a list of operator specifications using [<+>]. *)
end

type ('tok, 'out) parser
(** Values of that type are parsers from sequences of ['tok] to values of type
    ['out]. *)

val expression :
     appl:('b -> 'b -> 'b)
  -> token:('a -> 'b)
  -> ops:('a, 'b) Operators.t
  -> ('a, 'b) parser
(** [expression appl token ops] is a parser from sequences of ['a] to
    structured values of type ['b]. The parser is driven by the operator parser
    [ops] which determines which tokens are operators. Tokens that aren't
    operators are parsed using the [token] function.

    If tokens are seen as leaves of binary trees, the function [appl] is the
    concatenation of two binary trees. If tokens are seen as terms, [appl]
    is the application.

    For instance, assuming that [+] is declared infix and we're working with
    numbers, it can transform [3 + 5 × 2] encoded as the stream of terms [3, +,
    5, ×, 2] into the binary tree [@(@(×,@(@(+,3),5)),2)] where [@] denotes
    application nodes. *)

val run : ('tok, 'out) parser -> 'tok Seq.t -> ('out, 'tok) result
(** Run the given parser on a sequence of tokens. *)
