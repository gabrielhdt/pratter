(** {1 Setup} *)

type term = Terms.t

let symb, appl, add_args = Terms.(symb, appl, add_args)
let expr = Pratter.expression ~appl

let is (s : string) : term -> bool = function
  | Terms.Symb u when String.equal u s -> true
  | _ -> false

let symb_ (s : string) (t : term) : term option =
  if is s t then Some t else None

let token = Fun.id
let expr = expr ~token

(** {2 Alcotest setup} *)

let return, error = Result.(ok, error)

module RTTerm :
  Alcotest.TESTABLE with type t = (term, term Pratter.error) result = struct
  type t = (term, term Pratter.error) result

  let equal t u =
    match (t, u) with
    | Ok t, Ok u -> Terms.equal t u
    | Error `Too_few_arguments, Error `Too_few_arguments -> true
    | Error (`Op_conflict (t1, t2)), Error (`Op_conflict (u1, u2)) ->
        Terms.equal t1 u1 && Terms.equal t2 u2
    | _, _ -> false

  let pp oc t =
    match t with
    | Ok t -> Terms.pp oc t
    | Error `Too_few_arguments -> Format.pp_print_string oc "Too few arguments"
    | Error (`Op_conflict (t, u)) ->
        Format.fprintf oc "@[Operator conflict between@ \"%a\"@ and@ \"%a\"@]"
          Terms.pp t Terms.pp u
end

let rtterm :
    (module Alcotest.TESTABLE with type t = (term, term Pratter.error) result) =
  (module RTTerm)

(** {1 Simple tests} These tests define the behaviour of the Pratt parser. *)

let simple_infix () =
  let ops = Pratter.(Operators.infix (symb_ "+") Left 1.0) in
  let x = symb "x" in
  let y = symb "y" in
  let not_parsed = Stream.of_list [ x; symb "+"; y ] in
  let parsed = return @@ add_args (symb "+") [ x; y ] in
  Alcotest.check rtterm "x + y" (expr ~ops not_parsed) parsed

let two_operators () =
  let ops =
    Pratter.Operators.(
      infix (symb_ "+") Left 1.0 <+> infix (symb_ "*") Left 1.1)
  in
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
  Alcotest.check rtterm "x + y * z" parsed (expr ~ops not_parsed)

let appl_opertor () =
  let ops = Pratter.Operators.(infix (symb_ "+") Left 1.0) in
  let f = symb "f" in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ f; x; symb "+"; x ] in
  let parsed = return @@ add_args (symb "+") [ appl f x; x ] in
  Alcotest.check rtterm "f x + x" (expr ~ops not_parsed) parsed

let ops = Pratter.Operators.(prefix (symb_ "!") 1.0)

let simple_prefix () =
  let x = symb "x" in
  let not_parsed = Stream.of_list [ symb "!"; x ] in
  let parsed = return @@ appl (symb "!") x in
  Alcotest.check rtterm "! x" (expr ~ops not_parsed) parsed

let prefix_appl () =
  let x = symb "x" in
  let f = symb "f" in
  let not_parsed = Stream.of_list [ symb "!"; f; x ] in
  let parsed = return @@ appl (symb "!") (appl f x) in
  Alcotest.check rtterm "! f x" (expr ~ops not_parsed) parsed

let prefix_appl_in () =
  let x = symb "x" in
  let f = symb "f" in
  let fac = symb "!" in
  let not_parsed = Stream.of_list [ f; fac; x ] in
  let parsed =
    return
    @@
    let inside = appl fac x in
    appl f inside
  in
  Alcotest.check rtterm "f ! x = f (! x)" (expr ~ops not_parsed) parsed

let double_prefix () =
  (* !!x = !(!x) *)
  let not_parsed = Stream.of_list [ symb "!"; symb "!"; symb "x" ] in
  let parsed = return @@ appl (symb "!") (appl (symb "!") (symb "x")) in
  Alcotest.check rtterm "!!x" (expr ~ops not_parsed) parsed

let precedences_left_same () =
  (* x + y * z = (x + y) * z when bp(+) = bp( * ) and both are left
     associative *)
  let ops =
    let open Pratter.Operators in
    infix (symb_ "+") Left 1.0 <+> infix (symb_ "*") Left 1.0
  in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    return
    @@
    let left = add_args (symb "+") [ symb "x"; symb "y" ] in
    add_args (symb "*") [ left; symb "z" ]
  in
  Alcotest.check rtterm "x + y * z" (expr ~ops not_parsed) parsed

let precedences_right_same () =
  (* x + y * z = x + (y * z) when bp(+) = bp( * ) and both are right
     associative *)
  let ops =
    let open Pratter.Operators in
    infix (symb_ "+") Right 1.0 <+> infix (symb_ "*") Right 1.0
  in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    return
    @@
    let right = add_args (symb "*") [ symb "y"; symb "z" ] in
    add_args (symb "+") [ symb "x"; right ]
  in
  Alcotest.check rtterm "x + y * z" (expr ~ops not_parsed) parsed

let precedences_lt_not_assoc () =
  (* x + y * z = x + (y * z) when bp(+) < bp( * ) and both are not
     associative *)
  let ops =
    let open Pratter.Operators in
    infix (symb_ "+") Neither 0.0 <+> infix (symb_ "*") Neither 0.1
  in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    return
    @@
    let right = add_args (symb "*") [ symb "y"; symb "z" ] in
    add_args (symb "+") [ symb "x"; right ]
  in
  Alcotest.check rtterm "x + y * z" (expr ~ops not_parsed) parsed

let precedences_gt_not_assoc () =
  (* x + y * z = (x + y) * z when bp(+) > bp( * ) and both are not
     associative *)
  let ops =
    let open Pratter.Operators in
    infix (symb_ "+") Neither (-1.) <+> infix (symb_ "*") Neither (-1.1)
    (* NOTE that negative binding powers are accepted. *)
  in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    return
    @@
    let left = add_args (symb "+") [ symb "x"; symb "y" ] in
    add_args (symb "*") [ left; symb "z" ]
  in
  Alcotest.check rtterm "x + y * z" (expr ~ops not_parsed) parsed

(** Postfix *)

let postfix () =
  let ops = Pratter.Operators.(postfix (symb_ "!") 1.0) in
  let raw = Stream.of_list [ symb "x"; symb "!" ] in
  let parsed = return @@ add_args (symb "!") [ symb "x" ] in
  Alcotest.check rtterm "x ! = @(!, x)" parsed (expr ~ops raw);
  let raw = Stream.of_list [ symb "f"; symb "x"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "f") [ symb "x" ] ]
  in
  Alcotest.check rtterm "f x ! = @(!, @(f, x))" parsed (expr ~ops raw);
  let raw = Stream.of_list [ symb "x"; symb "!"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "!") [ symb "x" ] ]
  in
  Alcotest.check rtterm "x ! ! = @(!, @(!, x))" parsed (expr ~ops raw)

(** Mixes *)

let mixing_una_bin () =
  (* -x + y = (-x) + y when bp(-) > bp(+) *)
  let ops =
    Pratter.Operators.(infix (symb_ "+") Neither 1.0 <+> prefix (symb_ "-") 1.1)
  in
  let not_parsed = Stream.of_list [ symb "-"; symb "x"; symb "+"; symb "y" ] in
  let parsed =
    return @@ add_args (symb "+") [ add_args (symb "-") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check rtterm "(-x) + y" (expr ~ops not_parsed) parsed

let mixing_una_bin_bis () =
  (* !x + y = !(x + y) when bp(+) > bp(!) *)
  let ops =
    Pratter.Operators.(infix (symb_ "+") Neither 1.0 <+> prefix (symb_ "!") 0.9)
  in
  let not_parsed = Stream.of_list [ symb "!"; symb "x"; symb "+"; symb "y" ] in
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "+") [ symb "x"; symb "y" ] ]
  in
  Alcotest.check rtterm "!(x + y)" (expr ~ops not_parsed) parsed

let postfix_mixes_infix () =
  let ops =
    let open Pratter.Operators in
    postfix (symb_ "!") 1.0
    <+> infix (symb_ "+") Left 0.9
    <+> infix (symb_ "/") Left 1.1
  in
  let raw = Stream.of_list [ symb "x"; symb "!"; symb "+"; symb "y" ] in
  let parsed =
    return @@ add_args (symb "+") [ add_args (symb "!") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check rtterm "x ! + y = @(+, @(!, x), y)" (expr ~ops raw) parsed;
  let raw = Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "+") [ symb "x"; add_args (symb "!") [ symb "y" ] ]
  in
  Alcotest.check rtterm "x + y ! = @(+, x, @(!, y))" (expr ~ops raw) parsed

let postfix_mixes_prefix () =
  let ops =
    let open Pratter.Operators in
    postfix (symb_ "!") 1.0
    <+> prefix (symb_ "-") 1.1
    <+> prefix (symb_ "+") 0.9
  in
  let raw = Stream.of_list [ symb "-"; symb "x"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "!") [ add_args (symb "-") [ symb "x" ] ]
  in
  Alcotest.check rtterm "- x ! = @(!, @(-, x))" (expr ~ops raw) parsed;
  let raw = Stream.of_list [ symb "+"; symb "x"; symb "!" ] in
  let parsed =
    return @@ add_args (symb "+") [ add_args (symb "!") [ symb "x" ] ]
  in
  Alcotest.check rtterm "+ x ! = @(+, @(!, x))" (expr ~ops raw) parsed

let both_infix_prefix () =
  let ops =
    let open Pratter.Operators in
    prefix (symb_ "-") 2.0 <+> infix (symb_ "-") Left 1.0
  in
  let raw = Stream.of_list [ symb "-"; symb "x"; symb "-"; symb "y" ] in
  let parsed =
    return @@ add_args (symb "-") [ add_args (symb "-") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check rtterm "- x - y = @(@(-, @(-, x)), y)" parsed (expr ~ops raw)

(** Errors *)

let postfix_exn () =
  let ops =
    Pratter.Operators.(postfix (symb_ "!") 1.0 <+> prefix (symb_ "-") 1.0)
  in
  let raw = Stream.of_list [ symb "-"; symb "x"; symb "!" ] in
  Alcotest.check rtterm "- x !"
    (error @@ `Op_conflict (symb "x", symb "!"))
    (expr ~ops raw)

let precedences_eq_not_assoc () =
  (* x + y * z fails when bp(+) = bp( * ) and both are not associative *)
  let ops =
    Pratter.Operators.(
      infix (symb_ "+") Neither 1.0 <+> infix (symb_ "*") Neither 1.0)
  in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  Alcotest.check rtterm "x + y * z"
    (error @@ `Op_conflict (symb "y", symb "*"))
    (expr ~ops not_parsed)

let partial () =
  let ops =
    Pratter.Operators.(infix (symb_ "+") Left 1.0 <+> prefix (symb_ "!") 1.0)
  in
  let not_parsed = Stream.of_list [ symb "x"; symb "+" ] in
  Alcotest.check rtterm "x +" (expr ~ops not_parsed) (error `Too_few_arguments);
  Alcotest.check rtterm "!"
    (expr ~ops (Stream.of_list [ symb "!" ]))
    (error `Too_few_arguments)

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
               ; (2, map2 appl (self (n / 2)) (self (n / 2)))
               ]))

(** A {QCheck.arbitrary} for {b non-empty} term lists. *)
let term_list =
  let pp_sep ppf () = Format.fprintf ppf "; " in
  let print = Format.asprintf "[%a]" (Format.pp_print_list ~pp_sep Terms.pp) in
  let size = QCheck.Gen.(2 -- 100) in
  QCheck.(make ~print Gen.(list_size size term_gen))

let ops =
  let open Pratter.Operators in
  cat
    [
      infix (symb_ "+") Left 1.1
    ; infix (symb_ "*") Right 1.5
    ; infix (symb_ "=") Neither 0.5
    ; postfix (symb_ "!") 1.0
    ; prefix (symb_ "-") 0.9
    ]

(** Parsing does not change the number of leaf nodes of a term. *)
let constant_leaf_amount =
  (* Add some operators which can be generated by [string1_readable]. *)
  QCheck.(
    Test.make ~name:"parsing_constant_leaf_amount" ~count:100 term_list
      (fun l ->
        assume (l <> []);
        match expr ~ops (Stream.of_list l) with
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
        match expr ~ops (Stream.of_list l) with
        | Ok t -> Terms.depth t > List.(map Terms.depth l |> fold_left max 0)
        | Error _ -> true))

(** When there's no operator, parsing a list only builds an n-ary application.
 *)
let empty_table_id =
  QCheck.Test.make ~name:"parsing_identity_empty_table" ~count:100 term_list
    (fun l ->
      QCheck.assume (l <> []);
      let sequential_application =
        return @@ List.(fold_left appl (hd l) (tl l))
      in
      let parsed = expr ~ops:Pratter.Operators.none (Stream.of_list l) in
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
        let res = expr ~ops (Stream.of_list l) in
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
      ( "infix"
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
    ; ( "context dependent"
      , [ test_case "prefix infix" `Quick both_infix_prefix ] )
    ; ("errors", [ test_case "partial infix/prefix" `Quick partial ])
    ; ("properties", qsuite)
    ]
