(* This example shows how to parse a simple subset of arithmetics. In
   particular, we'll parse the expression [x! + y * -z].

   Terms are symbols and applications. The operators will be
   - [!] being postfix,
   - [+] being infix
   - [*] being infix with priority higher than [+] and
   - [-] being prefix. *)

(* We start by defining the terms *)
type term = Appl of term * term | Symb of string

(* then we declare some operators: for each operator, we declare whether
   they're infix, prefix or postfix and what's their precedence *)
let ops =
  let open Pratter.Operators in
  prefix (function Symb "-" as s -> Some s | _ -> None) 1.0
  <+> infix (function Symb "+" as s -> Some s | _ -> None) Left 0.5
  <+> infix (function Symb "*" as s -> Some s | _ -> None) Left 0.6
  <+> postfix (function Symb "!" as s -> Some s | _ -> None) 1.0

let appl x y = Appl (x, y)
let token x = x

(* And that's it! The function [arith] below is able to parse arithmetic
   expressions *)

let arith : term Stream.t -> (term, _) result =
  Pratter.expression ~token ~appl ~ops

let () =
  (* Let's try it, we'll parse the input [(x !) + (y * (-z))] represented as a
     flat stream of tokens *)
  let input : term list =
    [ "x"; "!"; "+"; "y"; "*"; "-"; "z" ] |> List.map (fun x -> Symb x)
    (* The [map] injects strings into the [term] type *)
  in
  (* and compare it to the following output, which is the same expression where
     the application is binary, prefix and explicit, for instance [- x] is
     written [Appl "-" x] (you might want to write it properly using a pen and
     a paper to convince yourself): *)
  let output : term =
    Appl
      ( Appl (Symb "+", Appl (Symb "!", Symb "x"))
      , Appl (Appl (Symb "*", Symb "y"), Appl (Symb "-", Symb "z")) )
  in
  assert (arith (Stream.of_list input) = Ok output)
