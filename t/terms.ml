module StrMap = Map.Make (String)

(** A simple term structure. *)
type t = Appl of t * t | Symb of string

(** General eliminator. *)
let rec elim : (string -> 'a) -> (t * t -> 'a -> 'a -> 'a) -> t -> 'a =
 fun rs ra t ->
  match t with
  | Symb s -> rs s
  | Appl (u, v) -> ra (u, v) (elim rs ra u) (elim rs ra v)

let on_symb : (string -> 'a option) -> t -> 'a option =
 fun rs -> elim rs (fun _ _ _ -> None)

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

(** [pp ppf t] pretty prints term [t] on formatter [oc]. *)
let rec pp ppf t =
  match t with
  | Appl (t, u) -> Format.fprintf ppf "@[<1>(%a@ %a)@]" pp t pp u
  | Symb id -> Format.pp_print_string ppf id

(** [count_symb t] returns the number of symbols in term [t]. *)
let count_symb : t -> int = elim (fun _ -> 1) (fun _ -> ( + ))

(** [depth t] returns the depth of term [t]. *)
let depth : t -> int = elim (fun _ -> 0) (fun _ dl dr -> 1 + max dl dr)
