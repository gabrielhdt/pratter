module StrMap = Map.Make (String)

(** A simple term structure. *)
type t = Appl of t * t | Symb of string

let appl t u = Appl (t, u)
let symb id = Symb id

(** [add_args t args] creates the application of [t] to the list of
    arguments [args]. *)
let rec add_args : t -> t list -> t =
 fun hd args ->
  match args with [] -> hd | a :: args -> add_args (Appl (hd, a)) args

(** Syntactic equality on terms. *)
let rec equal t u =
  match (t, u) with
  | Symb t, Symb u -> t = u
  | Appl (t, t'), Appl (u, u') -> equal t u && equal t' u'
  | _ -> false

(** [pp oc t] pretty prints term [t] on formatter [oc]. *)
let rec pp oc t =
  match t with
  | Appl (t, u) -> Format.fprintf oc "(%a %a)" pp t pp u
  | Symb id -> Format.pp_print_string oc id

(** [count_symb t] returns the number of symbols in term [t]. *)
let rec count_symb t : int =
  match t with Symb _ -> 1 | Appl (s, t) -> count_symb s + count_symb t

(** [depth t] returns the depth of term [t]. *)
let rec depth t : int =
  match t with Symb _ -> 0 | Appl (s, t) -> 1 + max (depth s) (depth t)
