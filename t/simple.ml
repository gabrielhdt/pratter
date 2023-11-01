(* Terms are made of symbols and applications and some symbol can
   be declared to be operators. *)

(* Start by defining the terms, *)
type term = Appl of term * term | Symb of string

(* and the data structure that maps symbols identifiers to operators
   properties, *)

type table = {
    prefix : (string * Pratter.priority) list
  ; infix : (string * (Pratter.priority * Pratter.associativity)) list
  ; postfix : (string * Pratter.priority) list
}

(* Next, define a module to pack these two types, and two functions:
   - `get` that is able to retrieve the properties of a symbol if it is
     declared as an operator,
   - `make_appl` that creates an application out of two terms. *)

module Terms : Pratter.SUPPORT with type term = term and type table = table =
struct
  type nonrec term = term
  type nonrec table = table

  let get { prefix; infix; postfix } (t : term) =
    match t with
    | Symb id -> (
        try Some (Pratter.Prefix, List.assoc id prefix)
        with Not_found -> (
          try
            let bp, assoc = List.assoc id infix in
            Some (Pratter.Infix assoc, bp)
          with Not_found -> (
            try Some (Pratter.Postfix, List.assoc id postfix)
            with Not_found -> None)))
    | _ -> None

  let make_appl t u = Appl (t, u)
end

module Parser = Pratter.Make (Terms)

(* Then that's it, we can parse streams of terms with operators. For instance,
   assume that we want to parse `x ! + y * -z` where

   - [+] and [*] are infix operators, with [*] having a higher binding power
     than [+];
   - [-] is a prefix operator having a higher binding power than [+] and [*],
   - [!] is a postfix operator. *)

(* Create a table holding these operators: *)

let tbl =
  {
    prefix = [ ("-", 1.0) ]
  ; infix = [ ("+", (0.5, Pratter.Left)); ("*", (0.6, Pratter.Left)) ]
  ; postfix = [ ("!", 1.0) ]
  }

(* Priority (also called binding power) can be any float, and associativity may
   be `Pratter.Left`, `Pratter.Right` or `Pratter.Neither`. *)

let () =
  (* Finally parse the input [(x !) + (y * (-z))] using `Parser.expression`: *)
  let input =
    [ Symb "x"; Symb "!"; Symb "+"; Symb "y"; Symb "*"; Symb "-"; Symb "z" ]
  in
  (* and check it gives the following output *)
  let output =
    Appl
      ( Appl (Symb "+", Appl (Symb "!", Symb "x"))
      , Appl (Appl (Symb "*", Symb "y"), Appl (Symb "-", Symb "z")) )
  in
  assert (Parser.expression tbl (Stream.of_list input) = Ok output)
