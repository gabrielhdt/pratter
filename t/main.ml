module StrMap = Map.Make (String)

type table = {
    prefix : Pratter.priority StrMap.t
  ; infix : (Pratter.priority * Pratter.associativity) StrMap.t
  ; postfix : Pratter.priority StrMap.t
}
(** Data structure containing the operators. *)

let empty : table =
  { prefix = StrMap.empty; infix = StrMap.empty; postfix = StrMap.empty }

(** A simple term structure. *)
type term = Appl of term * term | Symb of string

(** [symb id] creates a term from identifier [id]. *)
let symb id = Symb id

module Support : Pratter.SUPPORT with type term = term and type table = table =
struct
  type nonrec term = term
  type nonrec table = table

  let get { prefix; infix; postfix } t =
    match t with
    | Symb id -> (
        try Some (Pratter.Prefix, StrMap.find id prefix)
        with Not_found -> (
          try
            let bp, assoc = StrMap.find id infix in
            Some (Infix assoc, bp)
          with Not_found -> (
            try Some (Pratter.Postfix, StrMap.find id postfix)
            with Not_found -> None)))
    | _ -> None

  let make_appl t u = Appl (t, u)
end

module SupPrat = Pratter.Make (Support)

(** [add_args t args] creates the application of [t] to the list of
    arguments [args]. *)
let rec add_args : term -> term list -> term =
 fun hd args ->
  match args with
  | [] -> hd
  | a :: args -> add_args (Support.make_appl hd a) args

(** Module of testable terms for Alcotest. *)
module TTerm : Alcotest.TESTABLE with type t = term = struct
  type t = term

  (** Syntactic equality *)
  let rec equal t u =
    match (t, u) with
    | Symb t, Symb u -> t = u
    | Appl (t, t'), Appl (u, u') -> equal t u && equal t' u'
    | _ -> false

  let rec pp oc t =
    match t with
    | Appl (t, u) -> Format.fprintf oc "@(%a, %a)" pp t pp u
    | Symb id -> Format.pp_print_string oc id
end

let tterm : (module Alcotest.TESTABLE with type t = term) = (module TTerm)

let simple_infix () =
  let tbl = StrMap.add "+" (1.0, Pratter.Left) StrMap.empty in
  let tbl = { empty with infix = tbl } in
  let x = symb "x" in
  let y = symb "y" in
  let not_parsed = Stream.of_list [ x; symb "+"; y ] in
  let parsed = add_args (symb "+") [ x; y ] in
  Alcotest.(check tterm) "x + y" (SupPrat.expression tbl not_parsed) parsed

let two_operators () =
  let tbl =
    StrMap.(empty |> add "+" (1.0, Pratter.Left) |> add "*" (1.1, Pratter.Left))
  in
  let tbl = { empty with infix = tbl } in
  let x = symb "x" in
  let y = symb "y" in
  let z = symb "z" in
  let not_parsed = Stream.of_list [ x; symb "+"; y; symb "*"; z ] in
  let parsed =
    let right = add_args (symb "*") [ y; z ] in
    add_args (symb "+") [ x; right ]
  in
  Alcotest.(check tterm) "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let appl_opertor () =
  let tbl =
    { empty with infix = StrMap.add "+" (1.0, Pratter.Left) StrMap.empty }
  in
  let f = symb "f" in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ f; x; symb "+"; x ] in
  let parsed = add_args (symb "+") [ Support.make_appl f x; x ] in
  Alcotest.(check tterm) "f x + x" (SupPrat.expression tbl not_parsed) parsed

let simple_prefix () =
  let tbl = { empty with prefix = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ symb "!"; x ] in
  let parsed = Support.make_appl (symb "!") x in
  Alcotest.(check tterm) "! x" (SupPrat.expression tbl not_parsed) parsed

let prefix_appl () =
  let tbl = { empty with prefix = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let not_parsed = Stream.of_list [ symb "!"; f; x ] in
  let parsed = Support.(make_appl (symb "!") (make_appl f x)) in
  Alcotest.(check tterm) "! f x" (SupPrat.expression tbl not_parsed) parsed

let prefix_appl_in () =
  let tbl = { empty with prefix = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let fac = symb "!" in
  let not_parsed = Stream.of_list [ f; fac; x ] in
  let parsed =
    let inside = Support.make_appl fac x in
    Support.(make_appl f inside)
  in
  Alcotest.(check tterm)
    "f ! x = f (! x)"
    (SupPrat.expression tbl not_parsed)
    parsed

let double_prefix () =
  (* --x = -(-x) *)
  let tbl = { empty with prefix = StrMap.(add "-" 1.0 empty) } in
  let not_parsed = Stream.of_list [ symb "-"; symb "-"; symb "x" ] in
  let parsed = Appl (symb "-", Appl (symb "-", symb "x")) in
  Alcotest.check tterm "--x" (SupPrat.expression tbl not_parsed) parsed

let precedences_left_same () =
  (* x + y * z = (x + y) * z when bp(+) = bp( * ) and both are left
     associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (1.0, Left) |> add "*" (1.0, Left)))
  in
  let tbl = { empty with infix = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let left = add_args (symb "+") [ symb "x"; symb "y" ] in
    add_args (symb "*") [ left; symb "z" ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let precedences_right_same () =
  (* x + y * z = x + (y * z) when bp(+) = bp( * ) and both are right
     associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (1.0, Right) |> add "*" (1.0, Right)))
  in
  let tbl = { empty with infix = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let right = add_args (symb "*") [ symb "y"; symb "z" ] in
    add_args (symb "+") [ symb "x"; right ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let precedences_lt_not_assoc () =
  (* x + y * z = x + (y * z) when bp(+) < bp( * ) and both are not
     associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (0., Neither) |> add "*" (0.1, Neither)))
  in
  let tbl = { empty with infix = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let right = add_args (symb "*") [ symb "y"; symb "z" ] in
    add_args (symb "+") [ symb "x"; right ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let precedences_gt_not_assoc () =
  (* x + y * z = (x + y) * z when bp(+) > bp( * ) and both are not
     associative *)
  let tbl =
    Pratter.(
      StrMap.(empty |> add "+" (-1., Neither) |> add "*" (-1.1, Neither)))
    (* NOTE that negative binding powers are accepted. *)
  in
  let tbl = { empty with infix = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let left = add_args (symb "+") [ symb "x"; symb "y" ] in
    add_args (symb "*") [ left; symb "z" ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

(** Postfix *)

let postfix () =
  let tbl = { empty with postfix = StrMap.singleton "!" 1.0 } in
  let raw = Stream.of_list [ symb "x"; symb "!" ] in
  let parsed = add_args (symb "!") [ symb "x" ] in
  Alcotest.check tterm "x ! = @(!, x)" (SupPrat.expression tbl raw) parsed;
  let raw = Stream.of_list [ symb "f"; symb "x"; symb "!" ] in
  let parsed = add_args (symb "!") [ add_args (symb "f") [ symb "x" ] ] in
  Alcotest.check tterm "f x ! = @(!, @(f, x))"
    (SupPrat.expression tbl raw)
    parsed;
  let raw = Stream.of_list [ symb "x"; symb "!"; symb "!" ] in
  let parsed = add_args (symb "!") [ add_args (symb "!") [ symb "x" ] ] in
  Alcotest.check tterm "x ! ! = @(!, @(!, x))"
    (SupPrat.expression tbl raw)
    parsed

(** Mixes *)

let mixing_una_bin () =
  (* -x + y = (-x) + y when bp(-) > bp(+) *)
  let infix = StrMap.singleton "+" (1.0, Pratter.Neither) in
  let prefix = StrMap.singleton "-" 1.1 in
  let tbl = { empty with infix; prefix } in
  let not_parsed = Stream.of_list [ symb "-"; symb "x"; symb "+"; symb "y" ] in
  let parsed =
    add_args (symb "+") [ add_args (symb "-") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check tterm "(-x) + y" (SupPrat.expression tbl not_parsed) parsed

let mixing_una_bin_bis () =
  (* !x + y = !(x + y) when bp(+) > bp(!) *)
  let infix = StrMap.singleton "+" (1.0, Pratter.Neither) in
  let prefix = StrMap.singleton "!" 0.9 in
  let tbl = { empty with infix; prefix } in
  let not_parsed = Stream.of_list [ symb "!"; symb "x"; symb "+"; symb "y" ] in
  let parsed =
    add_args (symb "!") [ add_args (symb "+") [ symb "x"; symb "y" ] ]
  in
  Alcotest.check tterm "!(x + y)" (SupPrat.expression tbl not_parsed) parsed

let postfix_mixes_infix () =
  let tbl =
    {
      empty with
      postfix = StrMap.singleton "!" 1.0
    ; infix =
        StrMap.(
          singleton "+" (0.9, Pratter.Left) |> add "/" (1.1, Pratter.Left))
    }
  in
  let raw = Stream.of_list [ symb "x"; symb "!"; symb "+"; symb "y" ] in
  let parsed =
    add_args (symb "+") [ add_args (symb "!") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check tterm "x ! + y = @(+, @(!, x), y)"
    (SupPrat.expression tbl raw)
    parsed;
  let raw = Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "!" ] in
  let parsed =
    add_args (symb "+") [ symb "x"; add_args (symb "!") [ symb "y" ] ]
  in
  Alcotest.check tterm "x + y ! = @(+, x, @(!, y))"
    (SupPrat.expression tbl raw)
    parsed

let postfix_mixes_prefix () =
  let tbl =
    {
      empty with
      postfix = StrMap.singleton "!" 1.0
    ; prefix = StrMap.(singleton "-" 1.1 |> add "+" 0.9)
    }
  in
  let raw = Stream.of_list [ symb "-"; symb "x"; symb "!" ] in
  let parsed = add_args (symb "!") [ add_args (symb "-") [ symb "x" ] ] in
  Alcotest.check tterm "- x ! = @(!, @(-, x))"
    (SupPrat.expression tbl raw)
    parsed;
  let raw = Stream.of_list [ symb "+"; symb "x"; symb "!" ] in
  let parsed = add_args (symb "+") [ add_args (symb "!") [ symb "x" ] ] in
  Alcotest.check tterm "+ x ! = @(+, @(!, x))"
    (SupPrat.expression tbl raw)
    parsed

(** Errors *)

let postfix_exn () =
  let tbl =
    {
      empty with
      postfix = StrMap.singleton "!" 1.0
    ; prefix = StrMap.singleton "-" 1.0
    }
  in
  let raw = Stream.of_list [ symb "!"; symb "x" ] in
  Alcotest.check tterm "! x fails on !"
    (try
       ignore (SupPrat.expression tbl raw);
       assert false
     with SupPrat.UnexpectedPostfix t -> t)
    (symb "!");
  let raw = Stream.of_list [ symb "-"; symb "x"; symb "!" ] in
  Alcotest.check tterm "- x ! conflict"
    (try
       ignore (SupPrat.expression tbl raw);
       assert false
     with SupPrat.OpConflict (_, t) -> t)
    (symb "!")

let precedences_eq_not_assoc () =
  (* x + y * z fails when bp(+) = bp( * ) and both are not associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (1.0, Neither) |> add "*" (1.0, Neither)))
  in
  let tbl = { empty with infix = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  Alcotest.(check bool)
    "x + y * z"
    (try
       ignore (SupPrat.expression tbl not_parsed);
       false
     with SupPrat.OpConflict (_, _) -> true)
    true

let partial_infix () =
  let tbl = StrMap.singleton "+" (1.0, Pratter.Left) in
  let tbl = { empty with infix = tbl } in
  let not_parsed = Stream.of_list [ symb "x"; symb "+" ] in
  Alcotest.(check bool)
    "x +"
    (try
       ignore (SupPrat.expression tbl not_parsed);
       false
     with SupPrat.TooFewArguments -> true)
    true

let partial_prefix () =
  let tbl = StrMap.singleton "!" 1.0 in
  let tbl = { empty with prefix = tbl } in
  let not_parsed = Stream.of_list [ symb "!" ] in
  Alcotest.(check bool)
    "!"
    (try
       ignore (SupPrat.expression tbl not_parsed);
       false
     with SupPrat.TooFewArguments -> true)
    true

let bin_start_expr () =
  (* [+ x x] raises [UnexpectInfix +]: [+] has no left context. *)
  let tbl =
    { empty with infix = StrMap.singleton "+" (1.0, Pratter.Neither) }
  in
  let not_parsed = Stream.of_list [ symb "+"; symb "x"; symb "x" ] in
  Alcotest.check tterm "+ x x"
    (try
       ignore (SupPrat.expression tbl not_parsed);
       assert false
     with SupPrat.UnexpectedInfix t -> t)
    (symb "+")

let bin_bin () =
  (* x + + x raises [UnexpectInfix +]: the second [+] has no left context. *)
  let tbl =
    { empty with infix = StrMap.singleton "+" (1.0, Pratter.Neither) }
  in
  let not_parsed = Stream.of_list [ symb "x"; symb "+"; symb "+"; symb "x" ] in
  Alcotest.check tterm "x + + x"
    (try
       ignore (SupPrat.expression tbl not_parsed);
       assert false
     with SupPrat.UnexpectedInfix t -> t)
    (symb "+")

(** Qcheck tests *)

(** A generators for strings of length 1 made of printable ASCII characters. *)
let string1_readable = QCheck.Gen.(map (Printf.sprintf "%c") printable)

(** A generator for terms. Symbols names are generated by [string1_readable]. *)
let term_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
           if n = 0 then map symb string1_readable
           else
             frequency
               [
                 (1, map symb string1_readable)
               ; (2, map2 Support.make_appl (self (n / 2)) (self (n / 2)))
               ]))

(** A {QCheck.arbitrary} for {b non-empty} term lists. *)
let term_list =
  let pp_sep ppf () = Format.fprintf ppf "; " in
  let print = Format.asprintf "[%a]" (Format.pp_print_list ~pp_sep TTerm.pp) in
  let size = QCheck.Gen.(2 -- 100) in
  QCheck.(make ~print Gen.(list_size size term_gen))

(** [count_symb t] returns the number of symbols in term [t]. *)
let rec count_symb t : int =
  match t with Symb _ -> 1 | Appl (s, t) -> count_symb s + count_symb t

(** [depth t] returns the depth of term [t]. *)
let rec depth t : int =
  match t with Symb _ -> 0 | Appl (s, t) -> 1 + max (depth s) (depth t)

(** A sample table to be used in tests. *)
let table =
  let infix = StrMap.(add "+" (1.1, Pratter.Left) empty) in
  let infix = StrMap.(add "/" (1.5, Pratter.Left) infix) in
  let postfix = StrMap.(add "!" 1.0 empty) in
  let prefix = StrMap.(add "-" 0.9 empty) in
  { postfix; prefix; infix }

(** Parsing does not change the number of leaf nodes of a term. *)
let constant_leaf_amount =
  (* Add some operators which can be generated by [string1_readable]. *)
  QCheck.(
    Test.make ~name:"parsing_constant_leaf_amount" ~count:100 term_list
      (fun l ->
        assume (l <> []);
        try
          count_symb (SupPrat.expression table (Stream.of_list l))
          = List.fold_right (fun t -> ( + ) (count_symb t)) l 0
        with
        | SupPrat.UnexpectedInfix _ | SupPrat.UnexpectedPostfix _
        | SupPrat.TooFewArguments | SupPrat.OpConflict _
        ->
          true))

(** The depth of the parsed term is superior than the maximal depth of the input
    terms. *)
let greater_depth_parsed =
  QCheck.(
    Test.make ~name:"more_depth_in_parsed_exp" ~count:100 term_list (fun l ->
        assume (l <> []);
        try
          depth (SupPrat.expression table (Stream.of_list l))
          > List.(map depth l |> fold_left max 0)
        with
        | SupPrat.UnexpectedInfix _ | SupPrat.UnexpectedPostfix _
        | SupPrat.TooFewArguments | SupPrat.OpConflict _
        ->
          true))

(** When the table is empty, parsing a list only builds an n-ary application. *)
let empty_table_id =
  QCheck.Test.make ~name:"parsing_identity_empty_table" ~count:100 term_list
    (fun l ->
      QCheck.assume (l <> []);
      let sequential_application =
        List.(fold_left Support.make_appl (hd l) (tl l))
      in
      let parsed = SupPrat.expression empty (Stream.of_list l) in
      TTerm.equal sequential_application parsed)

let _ =
  let qsuite =
    List.map QCheck_alcotest.to_alcotest
      [ constant_leaf_amount; greater_depth_parsed; empty_table_id ]
  in
  let open Alcotest in
  run "Simple terms"
    [
      ("properties", qsuite)
    ; ( "infix"
      , [
          test_case "simple" `Quick simple_infix
        ; test_case "two" `Quick two_operators
        ; test_case "appl-bin" `Quick appl_opertor
        ; test_case "left assoc, same bp" `Quick precedences_left_same
        ; test_case "right assoc, same bp" `Quick precedences_right_same
        ; test_case "not assoc, lt bp" `Quick precedences_lt_not_assoc
        ; test_case "not assoc, gt bp" `Quick precedences_gt_not_assoc
        ; test_case "not assoc, same bp" `Quick precedences_eq_not_assoc
        ] )
    ; ( "prefix"
      , [
          test_case "simple" `Quick simple_prefix
        ; test_case "application head" `Quick prefix_appl
        ; test_case "application argument" `Quick prefix_appl_in
        ; test_case "double" `Quick double_prefix
        ] )
    ; ( "postfix"
      , [
          test_case "postfix" `Quick postfix
        ; test_case "mixes infix" `Quick postfix_mixes_infix
        ; test_case "mixes prefix" `Quick postfix_mixes_prefix
        ; test_case "exceptions" `Quick postfix_exn
        ] )
    ; ( "mixes"
      , [
          test_case "prefix infix" `Quick mixing_una_bin
        ; test_case "infix prefix" `Quick mixing_una_bin_bis
        ] )
    ; ( "errors"
      , [
          test_case "partial infix" `Quick partial_infix
        ; test_case "partial prefix" `Quick partial_prefix
        ; test_case "infix no left" `Quick bin_start_expr
        ; test_case "infix successive" `Quick bin_bin
        ] )
    ]
