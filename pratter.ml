(* Copyright (C) Gabriel Hondet.
   Subject to the BSD-3-Clause license *)

type associativity = Left | Right | Neither
type 't error = [ `OpConflict of 't * 't | `TooFewArguments ]
type 'a result = ('a, 'a error) Stdlib.result

let ( >> ) f g x = g (f x)

module Operators = struct
  type ('a, 'b) t = {
      prefix : 'a -> (float * 'b) option
    ; postfix : 'a -> (float * 'b) option
    ; infix : 'a -> (associativity * float * 'b) option
  }

  let none =
    {
      prefix = (fun _ -> None)
    ; postfix = (fun _ -> None)
    ; infix = (fun _ -> None)
    }

  let infix (parse : 'a -> 'b option) (a : associativity) (prio : float) :
      ('a, 'b) t =
    { none with infix = parse >> Option.map (fun p -> (a, prio, p)) }

  let postfix (parse : 'a -> 'b option) (prio : float) : ('a, 'b) t =
    { none with postfix = parse >> Option.map (fun p -> (prio, p)) }

  let prefix (parse : 'a -> 'b option) (prio : float) : ('a, 'b) t =
    { none with prefix = parse >> Option.map (fun p -> (prio, p)) }

  let ( <+> ) (o1 : ('a, 'b) t) (o2 : ('a, 'b) t) : ('a, 'b) t =
    let f is x = Option.(fold ~some ~none:(is o2 x) (is o1 x)) in
    {
      prefix = f (fun o -> o.prefix)
    ; postfix = f (fun o -> o.postfix)
    ; infix = f (fun o -> o.infix)
    }

  let cat (ops : ('a, 'b) t list) : ('a, 'b) t =
    List.fold_right ( <+> ) ops none
end

let expression (type a b) ~(appl : b -> b -> b) ~(token : a -> b)
    ~(ops : (a, b) Operators.t) : a Stream.t -> b result =
  (* [nud tbl strm t] (for null denotation) is the production of term [t] with
     {b no} left context. If [t] is not a prefix operator, [nud] is the
     identity. Otherwise, the output is a production rule. *)
  let rec nud strm (t : a) : b result =
    match ops.prefix t with
    | Some (rbp, t') ->
        Result.map (appl t') (expression ~rbp ~rassoc:Neither strm)
    | None -> Ok (token t)
  (* [led ~strm ~left t assoc bp] is the production of term [t] with
     left context [left]. We have the invariant that [t] is a binary operator
     with associativity [assoc] and binding power [bp]. This invariant is
     ensured while called in {!val:expression}. *)
  and led ~strm ~left t rassoc bp =
    let rbp =
      match rassoc with
      | Right -> bp *. (1. -. epsilon_float)
      | Left | Neither -> bp
    in
    Result.map (appl (appl t left)) (expression ~rbp ~rassoc strm)
  (* [expression ~rbp ~rassoc strm] parses next token of stream
     [strm] with previous operator having a right binding power [~rbp] and
     associativity [~rassoc]. *)
  and expression ~rbp ~rassoc strm =
    (* [aux left] inspects the stream and may consume one of its elements, or
       return [left] unchanged. *)
    let rec aux (left : b) =
      match Stream.peek strm with
      | None -> Ok left
      | Some pt -> (
          match ops.infix pt with
          | Some (lassoc, lbp, next) ->
              if lbp > rbp || (lbp = rbp && lassoc = Right && rassoc = Right)
              then
                (* Performed before to execute side effect on stream. *)
                let _ = Stream.next strm in
                Result.bind (led ~strm ~left next lassoc lbp) aux
              else if lbp < rbp || (lbp = rbp && lassoc = Left && rassoc = Left)
              then Ok left
              else Error (`OpConflict (left, next))
          | None -> (
              match ops.postfix pt with
              | Some (lbp, next) ->
                  if lbp > rbp then
                    (* Pop the term from the stream *)
                    let _ = Stream.next strm in
                    aux (appl next left)
                  else if lbp = rbp then Error (`OpConflict (left, next))
                  else Ok left
              | None ->
                  (* argument of an application *)
                  let _ = Stream.next strm in
                  Result.bind (nud strm pt) (fun right -> aux (appl left right))
              ))
    in
    try
      let next = Stream.next strm in
      let left = nud strm next in
      Result.bind left aux
    with Stream.Failure -> Error `TooFewArguments
  in
  expression ~rbp:neg_infinity ~rassoc:Neither
