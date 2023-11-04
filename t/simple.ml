(* This example shows how to parse a simple subset of arithmetics. In
   particular, we'll parse the expression [x! + y * -z].

   Terms are symbols and applications. The operators will be
   - [!] being postfix,
   - [+] being infix
   - [*] being infix with priority higher than [+] and
   - [-] being prefix. *)

(* We start by defining the terms *)
type term = Appl of term * term | Symb of string

(* along with a function that says which tokens are operators. For this, we
   define a data structure that holds our operators. *)

type table = {
    prefix : (string * float) list
  ; infix : (string * (float * Pratter.associativity)) list
  ; postfix : (string * float) list
}

(* We instantiate the data structure for our case. *)
let tbl =
  {
    prefix = [ ("-", 1.0) ]
  ; infix = [ ("+", (0.5, Pratter.Left)); ("*", (0.6, Pratter.Left)) ]
  ; postfix = [ ("!", 1.0) ]
  }

(* and we define the aforementioned function. *)
let is_op : term -> (Pratter.fixity * float) option = function
  | Symb id -> (
      try Some (Pratter.Prefix, List.assoc id tbl.prefix)
      with Not_found -> (
        try
          let bp, assoc = List.assoc id tbl.infix in
          Some (Pratter.Infix assoc, bp)
        with Not_found -> (
          try Some (Pratter.Postfix, List.assoc id tbl.postfix)
          with Not_found -> None)))
  | _ -> None

(* And that's it! The function [arith] below is able to parse arithmetic
   expressions *)

let arith : term Stream.t -> (term, _) result =
  Pratter.expression ~appl:(fun x y -> Appl (x, y)) ~is_op

let () =
  (* Let's try it, we'll parse the input [(x !) + (y * (-z))] represented as a
     flat stream of tokens *)
  let input : term list =
    [ Symb "x"; Symb "!"; Symb "+"; Symb "y"; Symb "*"; Symb "-"; Symb "z" ]
  in
  (* and compare it to the following output: *)
  let output : term =
    Appl
      ( Appl (Symb "+", Appl (Symb "!", Symb "x"))
      , Appl (Appl (Symb "*", Symb "y"), Appl (Symb "-", Symb "z")) )
  in
  assert (arith (Stream.of_list input) = Ok output)
