(* Copyright (C) Gabriel Hondet.
   Subject to the BSD-3-Clause license *)

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

module Operators = struct
  type 'a t = {
      is_prefix : 'a -> float option
    ; is_postfix : 'a -> float option
    ; is_infix : 'a -> (associativity * float) option
  }

  let none =
    {
      is_prefix = (fun _ -> None)
    ; is_postfix = (fun _ -> None)
    ; is_infix = (fun _ -> None)
    }

  let infix (is_op : 'a -> bool) (a : associativity) (prio : float) : 'a t =
    { none with is_infix = (fun t -> if is_op t then Some (a, prio) else None) }

  let postfix (is_op : 'a -> bool) (prio : float) : 'a t =
    { none with is_postfix = (fun t -> if is_op t then Some prio else None) }

  let prefix (is_op : 'a -> bool) (prio : float) : 'a t =
    { none with is_prefix = (fun t -> if is_op t then Some prio else None) }

  let ( <+> ) (o1 : 'a t) (o2 : 'a t) : 'a t =
    let f is x = Option.(fold ~some ~none:(is o2 x) (is o1 x)) in
    {
      is_prefix = f (fun o -> o.is_prefix)
    ; is_postfix = f (fun o -> o.is_postfix)
    ; is_infix = f (fun o -> o.is_infix)
    }

  let cat (ops : 'a t list) : 'a t = List.fold_right ( <+> ) ops none
end

(** [expression appl is_op s] parses the stream of tokens [s] and turns it into
    a full binary tree.

    If tokens are seen as leaves of binary trees, the function [appl] is the
    concatenation of two binary trees. If tokens are seen as terms, [appl]
    is the application.

    The function [is_op] is in charge of specifying which tokens are operators:
    for any term [t], [is_op t] must return [Some (f, p)] whenever [t] is an
    operator with fixity [f] and binding power (or precedence) [p]. If [t]
    isn't an operator, [is_op] should return [None].

    For instance, assuming that [+] is declared infix and we're working with
    numbers, it can transform [3 + 5 × 2] encoded as the stream of terms [3, +,
    5, ×, 2] into the binary tree [@(@(×,@(@(+,3),5)),2)] where [@] denotes
    nodes. *)
let expression :
       appl:('a -> 'a -> 'a)
    -> ops:'a Operators.t
    -> 'a Stream.t
    -> ('a, 'a error) result =
 fun ~appl ~ops ->
  (* [nud tbl strm t] (for null denotation) is the production of term [t] with
     {b no} left context. If [t] is not a prefix operator, [nud] is the
     identity. Otherwise, the output is a production rule. *)
  let rec nud strm t =
    match ops.is_prefix t with
    | Some rbp -> Result.map (appl t) (expression ~rbp ~rassoc:Neither strm)
    | None -> Ok t
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
          match ops.is_infix pt with
          | Some (lassoc, lbp) ->
              if lbp > rbp || (lbp = rbp && lassoc = Right && rassoc = Right)
              then
                (* Performed before to execute side effect on stream. *)
                let next = Stream.next strm in
                Result.bind (led ~strm ~left next lassoc lbp) aux
              else if lbp < rbp || (lbp = rbp && lassoc = Left && rassoc = Left)
              then Ok left
              else Error (`OpConflict (left, pt))
          | None -> (
              match ops.is_postfix pt with
              | Some lbp ->
                  if lbp > rbp then
                    let next = Stream.next strm in
                    aux (appl next left)
                  else if lbp = rbp then Error (`OpConflict (left, pt))
                  else Ok left
              | None ->
                  (* argument of an application *)
                  let next = Stream.next strm in
                  Result.bind (nud strm next) (fun right ->
                      aux (appl left right))))
    in
    try
      let next = Stream.next strm in
      let left = nud strm next in
      Result.bind left aux
    with Stream.Failure -> Error `TooFewArguments
  in
  expression ~rbp:neg_infinity ~rassoc:Neither
