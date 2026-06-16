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
let ops (t : term) : (Pratter.fixity * float * term) list =
  match t with
  | Symb "-" -> [ (Prefix, 1.0, t) ]
  | Symb "+" -> [ (Infix Left, 0.5, t) ]
  | Symb "*" -> [ (Infix Left, 0.6, t) ]
  | Symb "!" -> [ (Postfix, 1.0, t) ]
  | _ -> []

let appl x y = Appl (x, y)
let token x = x

(* And that's it! The function [arith] below is able to parse arithmetic
   expressions *)

let arith (terms : term Seq.t) : (term, _) result =
  Pratter.(run (expression ~token ~appl ~ops) terms)

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
  assert (arith (List.to_seq input) = Ok output)
