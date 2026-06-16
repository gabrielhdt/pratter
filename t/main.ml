(** {1 Setup} *)

(** {2 Pratter setup} *)

type term = Terms.t

let symb, appl, add_args = Terms.(symb, appl, add_args)

module StrMap = Map.Make (String)

type table = {
    prefix : Pratter.priority StrMap.t
  ; infix : (Pratter.priority * Pratter.associativity) StrMap.t
  ; postfix : Pratter.priority StrMap.t
}
(** Data structure containing the operators. *)

let empty : table =
  { prefix = StrMap.empty; infix = StrMap.empty; postfix = StrMap.empty }

module Support : Pratter.SUPPORT with type term = term and type table = table =
struct
  type nonrec term = term
  type nonrec table = table

  let get { prefix; infix; postfix } t =
    match t with
    | Terms.Symb id -> (
        try Some (Pratter.Prefix, StrMap.find id prefix)
        with Not_found -> (
          try
            let bp, assoc = StrMap.find id infix in
            Some (Infix assoc, bp)
          with Not_found -> (
            try Some (Pratter.Postfix, StrMap.find id postfix)
            with Not_found -> None)))
    | _ -> None

  let make_appl = appl
end

module SupPrat = Pratter.Make (Support)

(** {2 Alcotest setup} *)

let return, error = Result.(ok, error)

module RTTerm : Alcotest.TESTABLE with type t = (term, SupPrat.error) result =
struct
  type t = (Terms.t, SupPrat.error) result

  let equal t u =
    match (t, u) with
    | Ok t, Ok u
    | Error (`UnexpectedInfix t), Error (`UnexpectedInfix u)
    | Error (`UnexpectedPostfix t), Error (`UnexpectedPostfix u) ->
        Terms.equal t u
    | Error `TooFewArguments, Error `TooFewArguments -> true
    | Error (`OpConflict (t1, t2)), Error (`OpConflict (u1, u2)) ->
        Terms.equal t1 u1 && Terms.equal t2 u2
    | _, _ -> false

  let pp oc t =
    match t with
    | Ok t -> Terms.pp oc t
    | Error (`UnexpectedInfix t) ->
        Format.fprintf oc "Unexpected infix \"%a\"" Terms.pp t
    | Error (`UnexpectedPostfix t) ->
        Format.fprintf oc "Unexpected postfix \"%a\"" Terms.pp t
    | Error `TooFewArguments -> Format.pp_print_string oc "Too few arguments"
    | Error (`OpConflict (t, u)) ->
        Format.fprintf oc "Operator conflict between \"%a\" and \"%a\"" Terms.pp
          t Terms.pp u
end

let rtterm :
    (module Alcotest.TESTABLE with type t = (term, SupPrat.error) result) =
  (module RTTerm)

(** {1 Simple tests} These tests define the behaviour of the Pratt parser. *)

let simple_infix () =
  let tbl = StrMap.add "+" (1.0, Pratter.Left) StrMap.empty in
  let tbl = { empty with infix = tbl } in
  let x = symb "x" in
  let y = symb "y" in
  let not_parsed = Stream.of_list [ x; symb "+"; y ] in
  let parsed = return @@ add_args (symb "+") [ x; y ] in
  Alcotest.check rtterm "x + y" (SupPrat.expression tbl not_parsed) parsed

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
    return
    @@
    let right = add_args (symb "*") [ y; z ] in
    add_args (symb "+") [ x; right ]
  in
  Alcotest.check rtterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let appl_opertor () =
  let tbl =
    { empty with infix = StrMap.add "+" (1.0, Pratter.Left) StrMap.empty }
  in
  let f = symb "f" in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ f; x; symb "+"; x ] in
  let parsed = return @@ add_args (symb "+") [ Support.make_appl f x; x ] in
  Alcotest.check rtterm "f x + x" (SupPrat.expression tbl not_parsed) parsed

let simple_prefix () =
  let tbl = { empty with prefix = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ symb "!"; x ] in
  let parsed = return @@ Support.make_appl (symb "!") x in
  Alcotest.check rtterm "! x" (SupPrat.expression tbl not_parsed) parsed

let prefix_appl () =
  let tbl = { empty with prefix = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let not_parsed = Stream.of_list [ symb "!"; f; x ] in
  let parsed = return @@ Support.(make_appl (symb "!") (make_appl f x)) in
  Alcotest.check rtterm "! f x" (SupPrat.expression tbl not_parsed) parsed

let prefix_appl_in () =
  let tbl = { empty with prefix = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let fac = symb "!" in
  let not_parsed = Stream.of_list [ f; fac; x ] in
  let parsed =
    return
    @@
    let inside = Support.make_appl fac x in
    Support.(make_appl f inside)
  in
  Alcotest.check rtterm "f ! x = f (! x)"
    (SupPrat.expression tbl not_parsed)
    parsed

let double_prefix () =
  (* --x = -(-x) *)
  let tbl = { empty with prefix = StrMap.(add "-" 1.0 empty) } in
  let not_parsed = Stream.of_list [ symb "-"; symb "-"; symb "x" ] in
  let parsed = return @@ appl (symb "-") (appl (symb "-") (symb "x")) in
  Alcotest.check rtterm "--x" (SupPrat.expression tbl not_parsed) parsed

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
    return
    @@
    let left = add_args (symb "+") [ symb "x"; symb "y" ] in
    add_args (symb "*") [ left; symb "z" ]
  in
  Alcotest.check rtterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

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
    return
    @@
    let right = add_args (symb "*") [ symb "y"; symb "z" ] in
    add_args (symb "+") [ symb "x"; right ]
  in
  Alcotest.check rtterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

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
    return
    @@
    let right = add_args (symb "*") [ symb "y"; symb "z" ] in
    add_args (symb "+") [ symb "x"; right ]
  in
  Alcotest.check rtterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

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
    return
    @@
    let left = add_args (symb "+") [ symb "x"; symb "y" ] in
    add_args (symb "*") [ left; symb "z" ]
  in
  Alcotest.check rtterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

(** Postfix *)

let postfix () =
  let tbl = { empty with postfix = StrMap.singleton "!" 1.0 } in
  let raw = Stream.of_list [ symb "x"; symb "!" ] in
  let parsed = return @@ add_args (symb "!") [ symb "x" ] in
  Alcotest.check rtterm "x ! = @(!, x)" (SupPrat.expression tbl raw) parsed;
  let raw = Stream.of_list [ symb "f"; symb "x"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "f") [ symb "x" ] ]
  in
  Alcotest.check rtterm "f x ! = @(!, @(f, x))"
    (SupPrat.expression tbl raw)
    parsed;
  let raw = Stream.of_list [ symb "x"; symb "!"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "!") [ symb "x" ] ]
  in
  Alcotest.check rtterm "x ! ! = @(!, @(!, x))"
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
    return @@ add_args (symb "+") [ add_args (symb "-") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check rtterm "(-x) + y" (SupPrat.expression tbl not_parsed) parsed

let mixing_una_bin_bis () =
  (* !x + y = !(x + y) when bp(+) > bp(!) *)
  let infix = StrMap.singleton "+" (1.0, Pratter.Neither) in
  let prefix = StrMap.singleton "!" 0.9 in
  let tbl = { empty with infix; prefix } in
  let not_parsed = Stream.of_list [ symb "!"; symb "x"; symb "+"; symb "y" ] in
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "+") [ symb "x"; symb "y" ] ]
  in
  Alcotest.check rtterm "!(x + y)" (SupPrat.expression tbl not_parsed) parsed

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
    return @@ add_args (symb "+") [ add_args (symb "!") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check rtterm "x ! + y = @(+, @(!, x), y)"
    (SupPrat.expression tbl raw)
    parsed;
  let raw = Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "+") [ symb "x"; add_args (symb "!") [ symb "y" ] ]
  in
  Alcotest.check rtterm "x + y ! = @(+, x, @(!, y))"
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
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "-") [ symb "x" ] ]
  in
  Alcotest.check rtterm "- x ! = @(!, @(-, x))"
    (SupPrat.expression tbl raw)
    parsed;
  let raw = Stream.of_list [ symb "+"; symb "x"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "+") [ add_args (symb "!") [ symb "x" ] ]
  in
  Alcotest.check rtterm "+ x ! = @(+, @(!, x))"
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
  Alcotest.check rtterm "! x"
    (SupPrat.expression tbl raw)
    (error @@ `UnexpectedPostfix (symb "!"));
  let raw = Stream.of_list [ symb "-"; symb "x"; symb "!" ] in
  Alcotest.check rtterm "- x !"
    (SupPrat.expression tbl raw)
    (error @@ `OpConflict (symb "x", symb "!"))

let precedences_eq_not_assoc () =
  (* x + y * z fails when bp(+) = bp( * ) and both are not associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (1.0, Neither) |> add "*" (1.0, Neither)))
  in
  let tbl = { empty with infix = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  Alcotest.check rtterm "x + y * z"
    (error @@ `OpConflict (symb "y", symb "*"))
    (SupPrat.expression tbl not_parsed)

let partial () =
  let tbl =
    {
      empty with
      infix = StrMap.singleton "+" (1.0, Pratter.Left)
    ; prefix = StrMap.singleton "!" 1.0
    }
  in
  let not_parsed = Stream.of_list [ symb "x"; symb "+" ] in
  Alcotest.check rtterm "x +"
    (SupPrat.expression tbl not_parsed)
    (error `TooFewArguments);
  Alcotest.check rtterm "!"
    (SupPrat.expression tbl (Stream.of_list [ symb "!" ]))
    (error `TooFewArguments)

let bin_start_expr () =
  (* [+ x x] raises [UnexpectInfix +]: [+] has no left context. *)
  let tbl =
    { empty with infix = StrMap.singleton "+" (1.0, Pratter.Neither) }
  in
  let not_parsed = Stream.of_list [ symb "+"; symb "x"; symb "x" ] in
  Alcotest.check rtterm "+ x x"
    (SupPrat.expression tbl not_parsed)
    (error @@ `UnexpectedInfix (symb "+"))

let bin_bin () =
  (* x + + x raises [UnexpectInfix +]: the second [+] has no left context. *)
  let tbl =
    { empty with infix = StrMap.singleton "+" (1.0, Pratter.Neither) }
  in
  let not_parsed = Stream.of_list [ symb "x"; symb "+"; symb "+"; symb "x" ] in
  Alcotest.check rtterm "x + + x"
    (SupPrat.expression tbl not_parsed)
    (error @@ `UnexpectedInfix (symb "+"))

(** {1 Property-based tests with [QCheck]} *)

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
  let print = Format.asprintf "[%a]" (Format.pp_print_list ~pp_sep Terms.pp) in
  let size = QCheck.Gen.(2 -- 100) in
  QCheck.(make ~print Gen.(list_size size term_gen))

(** A sample table to be used in tests. *)
let table =
  let infix =
    StrMap.(
      empty
      |> add "+" (1.1, Pratter.Left)
      |> add "*" (1.5, Pratter.Right)
      |> add "=" (0.5, Pratter.Neither))
  in
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
        match SupPrat.expression table (Stream.of_list l) with
        | Ok t ->
            Terms.count_symb t
            = List.fold_right (fun t -> ( + ) (Terms.count_symb t)) l 0
        | Error _ -> true))

(** The depth of the parsed term is superior than the maximal depth of the input
    terms. *)
let greater_depth_parsed =
  QCheck.(
    Test.make ~name:"more_depth_in_parsed_exp" ~count:100 term_list (fun l ->
        assume (l <> []);
        match SupPrat.expression table (Stream.of_list l) with
        | Ok t -> Terms.depth t > List.(map Terms.depth l |> fold_left max 0)
        | Error _ -> true))

(** When the table is empty, parsing a list only builds an n-ary application. *)
let empty_table_id =
  QCheck.Test.make ~name:"parsing_identity_empty_table" ~count:100 term_list
    (fun l ->
      QCheck.assume (l <> []);
      let sequential_application =
        return @@ List.(fold_left Support.make_appl (hd l) (tl l))
      in
      let parsed = SupPrat.expression empty (Stream.of_list l) in
      RTTerm.equal sequential_application parsed)

(** {2 Compare against a YACC parser}*)

let symb_gen =
  let idgen = QCheck.Gen.(map (Printf.sprintf "%c") (char_range 'A' 'Z')) in
  let opgen = QCheck.Gen.oneofa [| "+"; "-"; "*"; "!" |] in
  QCheck.Gen.(map Terms.symb (frequency [ (4, idgen); (1, opgen) ]))

(** Generator for lists of symbols. *)
let slist =
  let size = QCheck.Gen.(2 -- 15) in
  let print =
    let pp_sep ppf () = Format.fprintf ppf "; " in
    Format.(asprintf "[%a]" (pp_print_list ~pp_sep Terms.pp))
  in
  QCheck.(make ~print Gen.(list_size size symb_gen))

let yacc_parse str =
  try Some (UarithGram.expr1 UarithLex.token (Lexing.from_string str))
  with Parsing.Parse_error -> None

let str_of_slist sl =
  sl |> List.map (Format.asprintf "%a" Terms.pp) |> String.concat " "

let compare_yacc =
  QCheck.(
    Test.make ~name:"compare_yacc" ~count:1000 slist (fun l ->
        let res = SupPrat.expression table (Stream.of_list l) in
        let expr = yacc_parse (str_of_slist l) in
        assume (Result.is_ok res && expr <> None);
        Option.get expr = Result.get_ok res))

let _ =
  let qsuite =
    List.map QCheck_alcotest.to_alcotest
      [
        constant_leaf_amount; greater_depth_parsed; empty_table_id; compare_yacc
      ]
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
          test_case "partial infix/prefix" `Quick partial
        ; test_case "infix no left" `Quick bin_start_expr
        ; test_case "infix successive" `Quick bin_bin
        ] )
    ]
