(* This example shows how to parse a simple subset of arithmetics. In
   particular, we'll parse the expression [x! + y * -z].

   Terms are symbols and applications. The operators will be
   - [!] being postfix,
   - [+] being infix
   - [*] being infix with priority higher than [+] and
   - [-] being prefix. *)

(* We start by defining the terms *)
type term = Appl of term * term | Symb of string

(* along with a function that says which tokens are operators, and for which
   operator, whether they're infix, prefix or postfix and what's their
   precedence.. *)
let is_op : term -> (Pratter.fixity * float) option = function
  | Symb "-" ->
      Some Pratter.(Prefix, 1.0) (* Unary prefix minus with precedence 1 *)
  | Symb "+" ->
      Some Pratter.(Infix Left, 0.5)
      (* Infix plus, associative to the left so that [3 + 5 + 2] is parsed
         [(3 + 5) + 2]. *)
  | Symb "*" ->
      Some Pratter.(Infix Left, 0.6)
      (* Infix times, with a higher precedence than [+], so that [3 + 5 * 2] is
         parsed [3 + (5 * 2)]. *)
  | Symb "!" -> Some Pratter.(Postfix, 1.0) (* Postfix factorial *)
  | _ -> None

(* And that's it! The function [arith] below is able to parse arithmetic
   expressions *)

let arith : term Stream.t -> (term, _) result =
  Pratter.expression ~appl:(fun x y -> Appl (x, y)) ~is_op

let () =
  (* Let's try it, we'll parse the input [(x !) + (y * (-z))] represented as a
     flat stream of tokens *)
  let input : term list =
    [ "x"; "!"; "+"; "y"; "*"; "-"; "z" ] |> List.map (fun x -> Symb x)
    (* The [map] injects strings into the [term] type *)
  in
  (* and compare it to the following output: *)
  let output : term =
    Appl
      ( Appl (Symb "+", Appl (Symb "!", Symb "x"))
      , Appl (Appl (Symb "*", Symb "y"), Appl (Symb "-", Symb "z")) )
  in
  assert (arith (Stream.of_list input) = Ok output)
