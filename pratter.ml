(* Copyright (C) 2021,2022 Gabriel Hondet.
   Subject to the BSD-3-Clause license *)

(** This module defines a functor whose image is a parser for terms with
    applications, infix, prefix or postfix operators. These terms are specified
    in the argument of the functor.

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
  | `UnexpectedInfix of 't
    (** An infix operator appears without left context. If [+] is an
        infix operator, it is raised in, e.g., [+ x x] or [x + + x x]. *)
  | `UnexpectedPostfix of 't
    (** A postfix operator appears without left context. If [!] is a
        postfix operator, it is raised in [! x]. *)
  | `TooFewArguments
    (** More arguments are expected. It is raised for instance on
        partial application of operators, such as [x +]. *)
  ]
(** Errors that can be encountered while parsing a stream of terms. *)

(** [expression appl is_op s] parses the stream of terms [s] and turns it into
    a single term.

    For that purpose, it uses the function [appl] such that for any terms [t]
    and [u], [appl t u] is the application of term [t] to term [u].

    The function [is_op] is in charge of specifying which terms are operators:
    for any term [t], [is_op t] must return [None] whenever [t] isn't an
    operator, and [Some (o, p)] whenever [t] is an operator with fixity [o] and
    binding power [p].

    For instance, assuming that [+] is declared infix and we're working with
    numbers, it can transform [3 + 5 + 2] encoded as the stream of terms [3, +,
    5, +, 2] into the term [(@(+(@(+,3,5)),2)] where [@] denotes the
    application. *)
let expression :
       appl:('a -> 'a -> 'a)
    -> is_op:('a -> (fixity * float) option)
    -> 'a Stream.t
    -> ('a, 'a error) result =
 fun ~appl ~is_op ->
  (* [nud tbl strm t] is the production of term [t] with {b no} left context.
     If [t] is not a prefix operator, [nud] is the identity. Otherwise, the
     output is a production rule. *)
  let rec nud strm t =
    match is_op t with
    | Some (Prefix, rbp) ->
        Result.map (appl t) (expression ~rbp ~rassoc:Neither strm)
    | Some (Infix _, _) -> Error (`UnexpectedInfix t)
    (* If the line above is erased, [+ x x] is parsed as [(+ x) x], and
       [x + + x x] as [(+ x) ((+ x) x)]. *)
    | Some (Postfix, _) -> Error (`UnexpectedPostfix t)
    | _ -> Ok t
  (* [led ~strm ~left t assoc bp] is the production of term [t] with
     left context [left]. We have the invariant that [t] is a binary operator
     with associativity [assoc] and binding power [bp]. This invariant is
     ensured while called in {!val:expression}. *)
  and led ~strm ~left t assoc bp =
    let rbp =
      match assoc with
      | Right -> bp *. (1. -. epsilon_float)
      | Left | Neither -> bp
    in
    Result.map (appl (appl t left)) (expression ~rbp ~rassoc:assoc strm)
  (* [expression ~rbp ~rassoc strm] parses next token of stream
     [strm] with previous operator having a right binding power [~rbp] and
     associativity [~rassoc]. *)
  and expression ~rbp ~rassoc strm =
    (* [aux left] inspects the stream and may consume one of its elements, or
       return [left] unchanged. *)
    let rec aux (left : 'a) =
      match Stream.peek strm with
      | None -> Ok left
      | Some pt -> (
          match is_op pt with
          | Some (Infix lassoc, lbp) ->
              if lbp > rbp || (lbp = rbp && lassoc = Right && rassoc = Right)
              then
                (* Performed before to execute side effect on stream. *)
                let next = Stream.next strm in
                Result.bind (led ~strm ~left next lassoc lbp) aux
              else if lbp < rbp || (lbp = rbp && lassoc = Left && rassoc = Left)
              then Ok left
              else Error (`OpConflict (left, pt))
          | Some (Postfix, lbp) ->
              if lbp > rbp then
                let next = Stream.next strm in
                aux (appl next left)
              else if lbp = rbp then Error (`OpConflict (left, pt))
              else Ok left
          | Some (Prefix, _) | None ->
              (* argument of an application *)
              let next = Stream.next strm in
              Result.bind (nud strm next) (fun right -> aux (appl left right)))
    in
    try
      let next = Stream.next strm in
      let left = nud strm next in
      Result.bind left aux
    with Stream.Failure -> Error `TooFewArguments
  in
  expression ~rbp:neg_infinity ~rassoc:Neither
