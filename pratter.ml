(* Copyright (C) Gabriel Hondet.
   Subject to the BSD-3-Clause license *)

let or_else (x : 'a option) (f : unit -> 'a) : 'a =
  match x with Some x -> x | None -> f ()

type associativity = Left | Right | Neither
type fixity = Infix of associativity | Prefix | Postfix
type 't error = [ `Op_conflict of 't | `Too_few_arguments ]
type ('a, 'b) result = ('a, 'b error) Stdlib.result

type ('tok, 'a) parser = 'tok Seq.t -> ('a * 'tok Seq.t, 'tok) result
(** Parses ['tok] to ['a]. *)

let pure (x : 'b) : ('a, 'b) parser = fun strm -> Ok (x, strm)

let bind (type a b) (p0 : ('tok, a) parser) (f : a -> ('tok, b) parser) :
    ('tok, b) parser =
 fun inp -> match p0 inp with Error _ as e -> e | Ok (out, inp') -> f out inp'

let fmap (f : 'a -> 'b) (p : ('tok, 'a) parser) : ('tok, 'b) parser =
 fun inp -> Result.map (fun (x, y) -> (f x, y)) (p inp)

let fail (e : 'i error) : ('i, _) parser = fun _ -> Error e
let push (x : 'i) : ('i, unit) parser = fun inp -> Ok ((), Seq.cons x inp)
(* Put back a token in the stream of input. *)

(** Parse a single token if there is one. *)
let tok : ('tok, 'tok option) parser =
 fun inp ->
  match inp () with
  | Seq.Nil -> Ok (None, Seq.empty)
  | Seq.Cons (t, rest) -> Ok (Some t, rest)

let ( let* ) = bind

let run (p : ('i, 'o) parser) (inp : _ Seq.t) : ('o, _) result =
  Result.map fst (p inp)

let on_prefix (f : float -> 'a -> 'b) : fixity * float * 'a -> 'b option =
  function
  | Prefix, prio, s -> Some (f prio s)
  | (Postfix | Infix _), _, _ -> None

let on_postfix (f : float -> 'a -> 'b) : fixity * float * 'a -> 'b option =
  function
  | Postfix, prio, s -> Some (f prio s)
  | (Prefix | Infix _), _, _ -> None

let on_infix (f : associativity -> float -> 'a -> 'b) :
    fixity * float * 'a -> 'b option = function
  | Infix assoc, prio, s -> Some (f assoc prio s)
  | (Postfix | Prefix), _, _ -> None

let expression (type a b) ~(appl : b -> b -> b) ~(token : a -> b)
    ~(ops : a -> (fixity * float * b) list) : (a, b) parser =
  (* [nud tbl strm t] (for null denotation) is the production of term [t] with
     {b no} left context. If [t] is not a prefix operator, [nud] is the
     identity. Otherwise, the output is a production rule. *)
  let rec nud (t : a) : (a, b) parser =
    or_else
      (List.find_map
         (on_prefix (fun rbp t' ->
              fmap (appl t') (expression ~rbp ~rassoc:Neither)))
         (ops t))
    @@ fun () -> pure (token t)
  (* [led ~strm ~left t assoc bp] is the production of term [t] with
      left context [left]. We have the invariant that [t] is a binary operator
      with associativity [assoc] and binding power [bp]. This invariant is
      ensured while called in {!val:expression}. *)
  and led ~left t rassoc bp : (a, b) parser =
    let rbp =
      match rassoc with
      | Right -> bp *. (1. -. epsilon_float)
      | Left | Neither -> bp
    in
    fmap (appl (appl t left)) (expression ~rbp ~rassoc)
  (* [expression ~rbp ~rassoc strm] parses next token of stream
     [strm] with previous operator having a right binding power [~rbp] and
     associativity [~rassoc]. *)
  and expression ~rbp ~rassoc =
    (* [aux left] inspects the stream and may consume one of its elements, or
       return [left] unchanged. *)
    let rec aux (left : b) : (a, b) parser =
      let* next = tok in
      match next with
      | None -> pure left
      | Some pt ->
          let ops = ops pt in
          (* Process the infix case *)
          let ifx lassoc lbp next =
            if lbp > rbp || (lbp = rbp && lassoc = Right && rassoc = Right) then
              let* t = led ~left next lassoc lbp in
              aux t
            else if lbp < rbp || (lbp = rbp && lassoc = Left && rassoc = Left)
            then
              (* Put back *)
              let* () = push pt in
              pure left
            else fail (`Op_conflict pt)
          in
          let postfix lbp next =
            if lbp > rbp then aux (appl next left)
            else if lbp = rbp then fail (`Op_conflict pt)
            else
              let* () = push pt in
              pure left
          in
          or_else (List.find_map (on_infix ifx) ops) @@ fun () ->
          or_else (List.find_map (on_postfix postfix) ops) @@ fun () ->
          let* right = nud pt in
          aux (appl left right)
    in
    let* next = tok in
    match next with
    | None -> fail `Too_few_arguments
    | Some next ->
        let* left = nud next in
        aux left
  in
  expression ~rbp:neg_infinity ~rassoc:Neither
