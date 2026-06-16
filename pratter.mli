(* Copyright (C) Gabriel Hondet.
   Subject to the BSD-3-Clause license *)

(** Transform strings of tokens and mixfix operators into full binary trees.
    Operators are characterised by their associativity and their fixity.

    To parse expressions of type ['a], you need to tell the parser
    - how to concatenate two expressions with a function of type
      ['a -> 'a -> 'a]; this function can be seen as the concatenation of two
      binary trees (and in that case, the input of the parser is a string of
      leaves);
    - how to determine whether a value of ['a] should be considered as an
      operator.

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

(** The fixity allows to determine where the arguments of an operator are. *)
type fixity =
  | Infix of associativity
      (** The operator is between its arguments, such as [=] in [x = y]. *)
  | Prefix  (** The operator is before its argument, such as [¬] in [¬ P]. *)
  | Postfix  (** The operator is after its argument, such as [²] in [x²]. *)

type 't error =
  [ `OpConflict of 't * 't
    (** Priority or associativiy conflict between two operators.
        In [`OpConflict (t,o)], [o] is an operator which generates a conflict
        preventing term [t] to be parsed. *)
  | `TooFewArguments
    (** More arguments are expected. It is raised for instance on
        partial application of operators, such as [x +]. *)
  ]
(** Errors that can be encountered while parsing a stream of terms. *)

module Operators : sig
  type 'a t
  (** The elements of that type drive the parser telling it which input tokens
      are operators. *)

  val none : _ t
  (** Tell the parser there's no operator. *)

  val infix : ('a -> bool) -> associativity -> float -> 'a t
  (** [infix is a pr] tells the parser that for any input [i], if [is i] is
      true, then it's an infix operator with associativity [a] and precedence
      [pr].

      For example, use [infix = Neither 10.] to consider the equality [=] as
      infix with no associativity so that you can parse [x = y]. *)

  val prefix : ('a -> bool) -> float -> 'a t
  (** [prefix is pr] tells the parser that for any input [i], if [is i] is true,
      then it's a prefix operator with precedence [pr].

      For example, use [prefix ¬ 1.] to consider the negation [¬] as a prefix
      operator so that you can parse [¬ x]. *)

  val postfix : ('a -> bool) -> float -> 'a t
  (** [postfix is pr] tells the parser that for any input [i], if [is i] is true,
      then it's a postfix operator with precedence [pr].

      For example, use [postfix ! 1.] to consider the factorial [!] as a postfix
      operator so that you can parse [x !]. *)

  val ( <+> ) : 'a t -> 'a t -> 'a t
  (** [o <+> p] tells the parser to consider the operators specified in [o] and
      in [p]. *)

  (** Note that through {!none} and {!(<+>)}, the type ['a t] is a {b semi-group}.
      You can use {!infix}, {!prefix} and {!postfix} to generate initial values
      and combine them with {!(<+>)}. *)

  val cat : 'a t list -> 'a t
  (** Concatenate a list of operator specifications using [<+>]. *)
end

val expression :
     appl:('a -> 'a -> 'a)
  -> ops:'a Operators.t
  -> 'a Stream.t
  -> ('a, 'a error) result
(** [expression appl ops s] parses the stream of tokens [s] and turns it into
    a full binary tree.

    If tokens are seen as leaves of binary trees, the function [appl] is the
    concatenation of two binary trees. If tokens are seen as terms, [appl]
    is the application.

    The structure [ops] is in charge of specifying which tokens are operators.

    For instance, assuming that [+] is declared infix and we're working with
    numbers, it can transform [3 + 5 × 2] encoded as the stream of terms [3, +,
    5, ×, 2] into the binary tree [@(@(×,@(@(+,3),5)),2)] where [@] denotes
    application nodes. *)
