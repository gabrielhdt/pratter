(** This modules defines a functor whose image is a parser for terms with
    applications, binary and unary operators. These terms are specified in the
    argument of the functor.

    The algorithm implemented is an extension of the Pratt parser. The Sunting
    Yard algorithm could also be used.
    @see <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
    @see <https://dev.to/jrop/pratt-parsing>
    @see <https://effbot.org/zone/simple-top-down-parsing.htm> *)

(** Associativity of an operator. *)
type associativity =
  | Left
      (** If [+] is a left associative operator, [x + y + z] is parsed [(x +
          y) + z]. *)
  | Right
      (** If [+] is a right associative operator, [x + y + z] is parsed [x +
          (y + z)]. *)
  | Neither
      (** If [+] is not associative, then [(x + y) + z] is not [x + (y + z)] and
          [x + y + z] results in a syntax error. *)

type priority = float
(** Priority of operators. If [*] has a higher priority than [+], than [x + y *
    z] is parsed [x + (y * z)]. *)

(** Types and utilities on terms that are to be Pratt parsed. *)
module type SUPPORT = sig
  type term
  (** The main type of terms, that contains symbols, applications, binary and
      unary operators. *)

  type table
  (** The table is used to store available operators. *)

  val get_unary : table -> term -> priority option
  (** [get_unary tbl t] returns the priority, or binding power of operator
      identified [t] if it is a unary operator, or [None]. *)

  val get_binary : table -> term -> (priority * associativity) option
  (** [get_binary tbl t] returns the priority (or binding power) and
      associativity of operator [t], or [None] if [t] isn't a binary
      operator. *)

  val make_appl : table -> term -> term -> term
  (** [make_appl tbl t u] returns the application of [t] to [u], sometimes noted
      [@(t, u)], or just [t u], with [tbl] being the table of operators. *)
end

module Make : functor (Sup : SUPPORT) -> sig
  exception ParseError of string * Sup.term
  (** [ParseError(s,t)] is raised when parsing term [t], and [s] gives the
      reason. *)

  val expression : Sup.table -> Sup.term Stream.t -> Sup.term
  (** [expression tbl s] parses stream of tokens [s] with table of operators
      [tbl]. It transforms a sequence of applications to a structured
      application tree containing infix and prefix operators. For instance,
      assuming that [+] is declared infix, it transforms [3 + 5 + 2],
      represented as [@(@(@(@(3,+),5),+),2)] (where [@] is the application) into
      [(@(+(@(+,3,5)),2)].

      @raises ParseError when input terms are ill-formed, with an explanation
              message. *)
end =
functor
  (Sup : SUPPORT)
  ->
  struct
    type table = Sup.table

    (** [lbp tbl t] returns the left binding power of term [t] (which is 0 if
        [t] is not an operator). *)
    let lbp : table -> Sup.term -> priority =
     fun tbl t ->
      match Sup.get_unary tbl t with
      | Some bp -> bp
      | None -> (
          match Sup.get_binary tbl t with
          | Some (bp, _) -> bp
          | None -> (* [t] must be an operator *) assert false )

    (** [is_assoc tbl t] returns [true] iff term [t] is associative in table
        [tbl]. *)
    let assoc : table -> Sup.term -> associativity =
     fun tbl t ->
      match Sup.get_binary tbl t with
      | Some (_, Left) -> Left
      | Some (_, Right) -> Right
      | Some (_, Neither) -> Neither
      | None -> assert false

    (* NOTE: among the four functions operating on streams, only [expression]
       consumes elements from it. *)

    (** [is_binop tbl t] returns [true] iff term [t] is a binary operator. *)
    let is_binop : table -> Sup.term -> bool =
     fun tbl t -> match Sup.get_binary tbl t with Some _ -> true | _ -> false

    exception ParseError of string * Sup.term

    (** [nud tbl strm t] is the production of term [t] with {b no} left context.
        If [t] is not an operator, [nud] is the identity. Otherwise, the output
        is a production rule. *)
    let rec nud : table -> Sup.term Stream.t -> Sup.term -> Sup.term =
     fun tbl strm t ->
      match Sup.get_unary tbl t with
      | Some rbp ->
          Sup.make_appl tbl t (expression ~tbl ~rbp ~rassoc:Neither strm)
      | _ -> t

    (** [led tbl left t] is the production of term [t] with left context
        [left]. *)
    and led : table -> Sup.term Stream.t -> Sup.term -> Sup.term -> Sup.term =
     fun tbl strm left t ->
      match Sup.get_binary tbl t with
      | Some (bp, assoc) ->
          let rbp =
            match assoc with
            | Right -> bp *. (1. -. epsilon_float)
            | Left | Neither -> bp
          in
          Sup.(
            make_appl tbl (make_appl tbl t left)
              (expression ~tbl ~rbp ~rassoc:assoc strm))
      | _ -> assert false

    (* [t] must be an operator. *)
    and expression :
           tbl:table
        -> rbp:priority
        -> rassoc:associativity
        -> Sup.term Stream.t
        -> Sup.term =
     fun ~tbl ~rbp ~rassoc strm ->
      (* [aux left] inspects the stream and may consume one of its elements, or
         return [left] unchanged. *)
      let rec aux left =
        match Stream.peek strm with
        | None -> left
        | Some pt when is_binop tbl pt ->
            (* If [pt] has a higher left binding power than the binding power of
               the previous operator in the stream. *)
            let lbp = lbp tbl pt in
            let lassoc = assoc tbl pt in
            if lbp > rbp || (lbp = rbp && lassoc = Right && rassoc = Right) then
              (* Performed before to execute side effect on stream. *)
              let next = Stream.next strm in
              aux (led tbl strm left next)
            else if lbp < rbp || (lbp = rbp && lassoc = Left && rassoc = Left)
            then left
            else
              let msg = "Not associative operators with same binding power" in
              raise (ParseError (msg, pt))
        | Some _ ->
            (* argument of an application *)
            let next = Stream.next strm in
            let right = nud tbl strm next in
            aux (Sup.make_appl tbl left right)
      in

      let next = Stream.next strm in
      let left = nud tbl strm next in
      aux left

    let expression : table -> Sup.term Stream.t -> Sup.term =
     fun tbl strm -> expression ~tbl ~rbp:0. ~rassoc:Neither strm
  end
