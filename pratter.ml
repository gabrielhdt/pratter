(** This modules defines a functor whose image is a parser for terms with
    applications, infix, prefix or postfix operators. These terms are specified
    in the argument of the functor.

    The algorithm implemented is an extension of the Pratt parser. The Shunting
    Yard algorithm could also be used.
    @see <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
    @see <https://dev.to/jrop/pratt-parsing> *)

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
(** Priority of operators, also called binding power. If [*] has a higher
    priority than [+], than [x + y * z] is parsed [x + (y * z)]. *)

(** A type to designate operators and their properties. *)
type operator =
  | Infix of associativity  (** Infix operator with an associativity. *)
  | Prefix
  | Postfix

(** Types and utilities on terms that are to be Pratt parsed. *)
module type SUPPORT = sig
  type term
  (** The main type of terms, that contains symbols, applications, infix, prefix
      or postfix operators. *)

  type table
  (** The table is used to store available operators. *)

  val get : table -> term -> (operator * priority) option
  (** [get tbl t] returns [None] if [t] is not an operator according to table
      [tbl], and it returns the properties of the operator otherwise. *)

  val make_appl : term -> term -> term
  (** [make_appl t u] returns the application of [t] to [u], sometimes noted
      [@(t, u)], or just [t u]. *)
end

module Make : functor (Sup : SUPPORT) -> sig
  type error =
    [ `OpConflict of Sup.term * Sup.term
      (** Priority or associativiy conflict between two operators.  In
          [`OpConflict (t,o)], operator [o] generates a conflict which prevents
          term [t] to be parsed. *)
    | `UnexpectedInfix of Sup.term
      (** An infix operator appears without left context. If [+] is an
          infix operator, it is raised in, e.g., [+ x x] or [x + + x x]. *)
    | `UnexpectedPostfix of Sup.term
      (** A postfix operator appears without left context. If [!] is a
          postfix operator, it is raised in [! x]. *)
    | `TooFewArguments
      (** More arguments are expected. It is raised for instance on
          partial application of operators, such as [x +]. *)
    ]
  (** Errors that can be encountered while parsing a stream of terms. *)

  val expression : Sup.table -> Sup.term Stream.t -> (Sup.term, error) result
  (** [expression tbl s] parses stream of tokens [s] with table of operators
      [tbl]. It transforms a sequence of applications to a structured
      application tree containing infix and prefix operators. For instance,
      assuming that [+] is declared infix, it transforms [3 + 5 + 2],
      represented as [@(@(@(@(3,+),5),+),2)] (where [@] is the application) into
      [(@(+(@(+,3,5)),2)]. *)
end =
functor
  (Sup : SUPPORT)
  ->
  struct
    type error =
      [ `OpConflict of Sup.term * Sup.term
      | `UnexpectedInfix of Sup.term
      | `UnexpectedPostfix of Sup.term
      | `TooFewArguments ]

    let return, error = Result.(ok, error)

    (* NOTE: among the four functions operating on streams, only [expression]
       consumes elements from it. *)

    (** [nud tbl strm t] is the production of term [t] with {b no} left context.
        If [t] is not a prefix operator, [nud] is the identity. Otherwise, the
        output is a production rule. *)
    let rec nud tbl strm t =
      match Sup.get tbl t with
      | Some (Prefix, rbp) ->
          Result.map (Sup.make_appl t)
            (expression ~tbl ~rbp ~rassoc:Neither strm)
      | Some (Infix _, _) -> error (`UnexpectedInfix t)
      (* If the line above is erased, [+ x x] is parsed as [(+ x) x], and
         [x + + x x] as [(+ x) ((+ x) x)]. *)
      | Some (Postfix, _) -> error (`UnexpectedPostfix t)
      | _ -> return t

    (** [led ~tbl ~strm ~left t assoc bp] is the production of term [t] with
        left context [left]. We have the invariant that [t] is a binary operator
        with associativity [assoc] and binding power [bp]. This invariant is
        ensured while called in {!val:expression}. *)
    and led ~tbl ~strm ~left t assoc bp =
      let rbp =
        match assoc with
        | Right -> bp *. (1. -. epsilon_float)
        | Left | Neither -> bp
      in
      Result.map
        Sup.(make_appl (make_appl t left))
        (expression ~tbl ~rbp ~rassoc:assoc strm)

    (** [expression ~tbl ~rbp ~rassoc strm] parses next token of stream
        [strm] with previous operator having a right binding power [~rbp] and
        associativity [~rassoc]. *)
    and expression ~tbl ~rbp ~rassoc strm =
      (* [aux left] inspects the stream and may consume one of its elements, or
         return [left] unchanged. *)
      let rec aux (left : Sup.term) =
        match Stream.peek strm with
        | None -> return left
        | Some pt -> (
            match Sup.get tbl pt with
            | Some (Infix lassoc, lbp) ->
                if lbp > rbp || (lbp = rbp && lassoc = Right && rassoc = Right)
                then
                  (* Performed before to execute side effect on stream. *)
                  let next = Stream.next strm in
                  Result.bind (led ~tbl ~strm ~left next lassoc lbp) aux
                else if
                  lbp < rbp || (lbp = rbp && lassoc = Left && rassoc = Left)
                then return left
                else error (`OpConflict (left, pt))
            | Some (Postfix, lbp) ->
                if lbp > rbp then
                  let next = Stream.next strm in
                  aux (Sup.make_appl next left)
                else if lbp = rbp then error (`OpConflict (left, pt))
                else return left
            | Some (Prefix, _) | None ->
                (* argument of an application *)
                let next = Stream.next strm in
                Result.bind (nud tbl strm next) (fun right ->
                    aux (Sup.make_appl left right)))
      in

      try
        let next = Stream.next strm in
        let left = nud tbl strm next in
        Result.bind left aux
      with Stream.Failure -> error `TooFewArguments

    let expression tbl = expression ~tbl ~rbp:neg_infinity ~rassoc:Neither
  end
