include module type of struct include Usexp end with module Loc := Usexp.Loc

module To_sexp : sig
  type sexp = t
  include Sexp_intf.Combinators.S with type 'a t = 'a -> t

  val record : (string * sexp) list -> sexp

  type field

  val field
    :  string
    -> 'a t
    -> ?equal:('a -> 'a -> bool)
    -> ?default:'a
    -> 'a
    -> field
  val field_o : string -> 'a t-> 'a option -> field

  val record_fields : field list t

  val unknown : _ t
end with type sexp := t

module Loc = Usexp.Loc

module Of_sexp : sig
  type ast = Ast.t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | Template of Template.t
    | List of Loc.t * ast list

  type hint =
    { on: string
    ; candidates: string list
    }

  exception Of_sexp of Loc.t * string * hint option

  (** Monad producing a value of type ['a] by parsing an input
      composed of a sequence of S-expressions.

      The input can be seen either as a plain sequence of
      S-expressions or a list of fields. The ['kind] parameter
      indicates how the input is seen:

      - with {['kind = [values]]}, the input is seen as an ordered
      sequence of S-expressions

      - with {['kind = [fields]]}, the input is seen as an unordered
      sequence of fields

      A field is a S-expression of the form: [(<atom> <values>...)]
      where [atom] is a plain atom, i.e. not a quoted string and not
      containing variables. [values] is a sequence of zero, one or more
      S-expressions.

      It is possible to switch between the two mode at any time using
      the appropriate combinator. Some primitives can be used in both
      mode while some are specific to one mode.  *)
  type ('a, 'kind, 'ctx) parser

  type values
  type fields

  type ('a, 'ctx) t             = ('a, values, 'ctx) parser
  type ('a, 'ctx) fields_parser = ('a, fields, 'ctx) parser

  (** [parse parser context sexp] parse a S-expression using the
      following parser. The input consist of a single
      S-expression. [context] allows to pass extra information such as
      versions to individual parsers. *)
  val parse : ('a, 'ctx) t -> 'ctx -> ast -> 'a

  val return : 'a -> ('a, _, _) parser

  val (>>=)
    :  ('a, 'k, 'ctx) parser
    -> ('a -> ('b, 'k, 'ctx) parser)
    -> ('b, 'k, 'ctx) parser

  val (>>|) : ('a, 'k, 'ctx) parser -> ('a -> 'b) -> ('b, 'k, 'ctx) parser

  val (>>>)
    :  (unit, 'k, 'ctx) parser
    -> ('a, 'k, 'ctx) parser
    -> ('a, 'k, 'ctx) parser

  val map : ('a, 'k, 'ctx) parser -> f:('a -> 'b) -> ('b, 'k, 'ctx) parser

  val try_
    :  ('a, 'k, 'ctx) parser
    -> (exn -> ('a, 'k, 'ctx) parser)
    -> ('a, 'k, 'ctx) parser

  (** Access to the context *)
  val get : 'a Univ_map.Key.t -> ('a option, _, Univ_map.t) parser

  val set
    :  'a Univ_map.Key.t
    -> 'a
    -> ('b, 'k, Univ_map.t) parser
    -> ('b, 'k, Univ_map.t) parser

  val get_all : ('ctx, _, 'ctx) parser
  val set_many
    :  Univ_map.t
    -> ('a, 'k, Univ_map.t) parser
    -> ('a, 'k, Univ_map.t) parser

  (** Return the location of the list currently being parsed. *)
  val loc : (Loc.t, _, _) parser

  (** End of sequence condition. Uses [then_] if there are no more
      S-expressions to parse, [else_] otherwise. *)
  val if_eos
    :  then_:('a, 'b, 'ctx) parser
    -> else_:('a, 'b, 'ctx) parser
    -> ('a, 'b, 'ctx) parser

  (** If the next element of the sequence is a list, parse it with
      [then_], otherwise parse it with [else_]. *)
  val if_list
    :  then_:('a, 'ctx) t
    -> else_:('a, 'ctx) t
    -> ('a, 'ctx) t

  (** If the next element of the sequence is of the form [(:<name>
     ...)], use [then_] to parse [...]. Otherwise use [else_]. *)
  val if_paren_colon_form
    :  then_:(Loc.t * string -> 'a, 'ctx) t
    -> else_:('a, 'ctx) t
    -> ('a, 'ctx) t

  (** Expect the next element to be the following atom. *)
  val keyword : string -> (unit, _) t

  (** {[match_keyword [(k1, t1); (k2, t2); ...] ~fallback]} inspects
     the next element of the input sequence. If it is an atom equal to
     one of [k1], [k2], ... then the corresponding parser is used to
     parse the rest of the sequence. Other [fallback] is used. *)
  val match_keyword
    :  (string * ('a, 'ctx) t) list
    -> fallback:('a, 'ctx) t
    -> ('a, 'ctx) t

  (** Use [before] to parse elements until the keyword is
      reached. Then use [after] to parse the rest. *)
  val until_keyword
    :  string
    -> before:('a, 'ctx) t
    -> after:('b, 'ctx) t
    -> ('a list * 'b option, 'ctx) t

  (** What is currently being parsed. The second argument is the atom
      at the beginnig of the list when inside a [sum ...] or [field
      ...]. *)
  type kind =
    | Values of Loc.t * string option
    | Fields of Loc.t * string option
  val kind : (kind, _, _) parser

  (** [repeat t] use [t] to consume all remaning elements of the input
      until the end of sequence is reached. *)
  val repeat : ('a, 'ctx) t -> ('a list, 'ctx) t

  (** Capture the rest of the input for later parsing *)
  val capture : (('a, 'ctx) t -> 'a, 'ctx) t

  (** [enter t] expect the next element of the input to be a list and
      parse its contents with [t]. *)
  val enter : ('a, 'ctx) t -> ('a, 'ctx) t

  (** [fields fp] converts the rest of the current input to a list of
      fields and parse them with [fp]. This operation fails if one the
      S-expression in the input is not of the form [(<atom>
      <values>...)] *)
  val fields : ('a, 'ctx) fields_parser -> ('a, 'ctx) t

  (** [record fp = enter (fields fp)] *)
  val record : ('a, 'ctx) fields_parser -> ('a, 'ctx) t

  (** Consume the next element of the input as a string, int, char, ... *)
  include Sexp_intf.Combinators.S2 with type ('a, 'ctx) t := ('a, 'ctx) t

  (** Unparsed next element of the input *)
  val raw : (ast, _) t

  (** Inspect the next element of the input without consuming it *)
  val peek : (ast option, _) t

  (** Same as [peek] but fail if the end of input is reached *)
  val peek_exn : (ast, _) t

  (** Consume and ignore the next element of the input *)
  val junk : (unit, _) t

  (** Ignore all the rest of the input *)
  val junk_everything : (unit, _, _) parser

  (** [plain_string f] expects the next element of the input to be a
      plain string, i.e. either an atom or a quoted string, but not a
      template nor a list. *)
  val plain_string : (loc:Loc.t -> string -> 'a) -> ('a, _) t

  val fix : (('a, 'ctx) t -> ('a, 'ctx) t) -> ('a, 'ctx) t

  val of_sexp_error
    :  ?hint:hint
    -> Loc.t
    -> string
    -> _
  val of_sexp_errorf
    :  ?hint:hint
    -> Loc.t
    -> ('a, unit, string, 'b) format4
    -> 'a

  val no_templates
    : ?hint:hint
    -> Loc.t
    -> ('a, unit, string, 'b) format4
    -> 'a

  val located : ('a, 'k, 'ctx) parser -> (Loc.t * 'a, 'k, 'ctx) parser

  val enum : (string * 'a) list -> ('a, _) t

  (** Parser that parse a S-expression of the form [(<atom> <s-exp1>
      <s-exp2> ...)] or [<atom>]. [<atom>] is looked up in the list and
      the remaining s-expressions are parsed using the corresponding
      list parser. *)
  val sum : (string * ('a, 'ctx) t) list -> ('a, 'ctx) t

  (** Check the result of a list parser, and raise a properly located
      error in case of failure. *)
  val map_validate
    :  ('a, 'ctx) fields_parser
    -> f:('a -> ('b, string) Result.t)
    -> ('b, 'ctx) fields_parser

  (** {3 Parsing record fields} *)

  val field
    :  string
    -> ?default:'a
    -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
    -> ('a, 'ctx) t
    -> ('a, 'ctx) fields_parser

  val field_o
    :  string
    -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
    -> ('a, 'ctx) t
    -> ('a option, 'ctx) fields_parser

  val field_b
    :  ?check:((unit, 'ctx) t)
    -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
    -> string
    -> (bool, 'ctx) fields_parser

  (** A field that can appear multiple times *)
  val multi_field
    :  string
    -> ('a, 'ctx) t
    -> ('a list, 'ctx) fields_parser

  (** Default value for [on_dup]. It fails with an appropriate error
      message. *)
  val field_present_too_many_times : Univ_map.t -> string -> Ast.t list -> _

  module Let_syntax : sig
    val ( $ )
      :  ('a -> 'b, 'k, 'ctx) parser
      -> ('a, 'k, 'ctx) parser
      -> ('b, 'k, 'ctx) parser

    val const : 'a -> ('a, _, _) parser
  end
end

module type Sexpable = sig
  type t
  val t : (t, _) Of_sexp.t
  val sexp_of_t : t To_sexp.t
end
