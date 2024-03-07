(** Reading OCaml values from dune lang ones *)

open Stdune

(** Monad producing a value of type ['a] by parsing an input composed of a
    sequence of S-expressions.

    The input can be seen either as a plain sequence of S-expressions or a list
    of fields. The ['kind] parameter indicates how the input is seen:

    - with ['kind = [values]], the input is seen as an ordered sequence of
      S-expressions

    - with [!'kind = [fields]], the input is seen as an unordered sequence of
      fields

    A field is a S-expression of the form: [(<atom> <values>...)] where [atom]
    is a plain atom, i.e. not a quoted string and not containing variables.
    [values] is a sequence of zero, one or more S-expressions.

    It is possible to switch between the two mode at any time using the
    appropriate combinator. To switch from [values] to [fields], use [field]. To
    switch from [fields] to [values], use [fields]. Some primitives can be used
    in both mode while some are specific to one mode. *)
type ('a, 'kind) parser

type values
type fields
type 'a t = ('a, values) parser
type 'a fields_parser = ('a, fields) parser

(** [parse parser context sexp] parse a S-expression using the following parser.
    The input consist of a single S-expression. [context] allows to pass extra
    information such as versions to individual parsers. *)
val parse : 'a t -> Univ_map.t -> Ast.t -> 'a

(** [return] is a parser that returns the given value. *)
val return : 'a -> ('a, _) parser

(** [map t ~f] parses the input using [t] and then applies [f] to the result. *)
val map : ('a, 'k) parser -> f:('a -> 'b) -> ('b, 'k) parser

(** [t >>= f] parses the input using [t] and then uses [f] to parse the rest of
    the input. *)
val ( >>= ) : ('a, 'k) parser -> ('a -> ('b, 'k) parser) -> ('b, 'k) parser

(** [t >>| f] is [map t ~f]. *)
val ( >>| ) : ('a, 'k) parser -> ('a -> 'b) -> ('b, 'k) parser

(** [t >>> u] parses the input using [t] which returns [unit] and then parses
    the rest of the input using [u]. *)
val ( >>> ) : (unit, 'k) parser -> ('a, 'k) parser -> ('a, 'k) parser

(** [( let* ) t f] is [t >>= fun x -> f x]. *)
val ( let* ) : ('a, 'k) parser -> ('a -> ('b, 'k) parser) -> ('b, 'k) parser

(** [( let+ ) t f] is [t >>| fun x -> f x]. *)
val ( let+ ) : ('a, 'k) parser -> ('a -> 'b) -> ('b, 'k) parser

(** [and+ t u] parses the input using [t] and then [u] and returns a pair of the
    two results. This is the same as [pair]. *)
val ( and+ ) : ('a, 'k) parser -> ('b, 'k) parser -> ('a * 'b, 'k) parser

(** [try_ t f] is [t] if [t] succeeds and parses with [f] if [t] raises an
    exception. *)
val try_ : ('a, 'k) parser -> (exn -> ('a, 'k) parser) -> ('a, 'k) parser

(** [traverse l ~f] parses the input using [f] for each element of [l] and
    returns the list of results. *)
val traverse : 'a list -> f:('a -> ('b, 'k) parser) -> ('b list, 'k) parser

(** [all l] parses the input using each parser in [l] and returns the list of
    results. *)
val all : ('a, 'k) parser list -> ('a list, 'k) parser

(** [get key] gets the parser associated with the context [key]. *)
val get : 'a Univ_map.Key.t -> ('a option, _) parser

(** [set key t] sets the parser associated with the context [key] to [t]. *)
val set : 'a Univ_map.Key.t -> 'a -> ('b, 'k) parser -> ('b, 'k) parser

(** [get_all] gets the whole context. *)
val get_all : (Univ_map.t, _) parser

(** [set_many context t] applies the [context] to the parser [t]. *)
val set_many : Univ_map.t -> ('a, 'k) parser -> ('a, 'k) parser

(** [update_var key ~f t] applies [f] to the value associated with [key] in the
    context and then parses the input using [t] together with the updated
    context. *)
val update_var
  :  'a Univ_map.Key.t
  -> f:('a option -> 'a option)
  -> ('b, 'k) parser
  -> ('b, 'k) parser

(** Return the location of the list currently being parsed. *)
val loc : (Loc.t, _) parser

(** [a <|> b] is either [a] or [b]. If [a] fails to parse the input, then try
    [b]. If [b] fails as well, raise the error from the parser that consumed the
    most input. *)
val ( <|> ) : ('a, 'k) parser -> ('a, 'k) parser -> ('a, 'k) parser

(** [either a b] is like [a <|> b] but it also informs you which parser
    succeeded. *)
val either : ('a, 'k) parser -> ('b, 'k) parser -> (('a, 'b) Either.t, 'k) parser

(** [atom_matching f] expects the next element to be an atom for which [f]
    returns [Some v]. [desc] is used to describe the atom in case of error. [f]
    must not raise. *)
val atom_matching : (string -> 'a option) -> desc:string -> 'a t

(** [keyword s] is a short-hand for

    {[
      atom_matching (String.equal s) ~desc:(sprintf "'%s'" s)
    ]} *)
val keyword : string -> unit t

(** Use [before] to parse elements until the keyword is reached. Then use
    [after] to parse the rest. *)
val until_keyword : string -> before:'a t -> after:'b t -> ('a list * 'b option) t

(** What is currently being parsed. The second argument is the atom at the
    beginning of the list when inside a [sum ...] or [field ...]. *)
type kind =
  | Values of Loc.t * string option
  | Fields of Loc.t * string option

(** [kind] returns the current kind of the parser, whether it is parsing a list
    of values or a list of fields, together with the atom at the beginning of
    a list when inside [sum ...] or [field ...]. *)
val kind : (kind, _) parser

(** [repeat t] uses [t] to consume all remaining elements of the input until the
    end of sequence is reached. *)
val repeat : 'a t -> 'a list t

(** Like [repeat] but the list of elements must be non-empty. *)
val repeat1 : 'a t -> 'a list t

(** Capture the rest of the input for later parsing. *)
val capture : ('a t -> 'a) t

(** Delay the parsing of the rest of the input. *)
val lazy_ : 'a t -> 'a Lazy.t t

(** [enter t] expect the next element of the input to be a list and parse its
    contents with [t]. *)
val enter : 'a t -> 'a t

(** [fields fp] converts the rest of the current input to a list of fields and
    parse them with [fp]. This operation fails if one the S-expression in the
    input is not of the form [(<atom> <values>...)]. *)
val fields : 'a fields_parser -> 'a t

(** Consume the next element of the input as a [string]. *)
val string : string t

(** Consume the next element of the input as an [int]. *)
val int : int t

(** Consume the next element of the input as a [float]. *)
val float : float t

(** Consume the next element of the input as a [bool]. *)
val bool : bool t

(** [pair a b] consumes two elements sequentially using [a] and then [b] to
    parse. *)
val pair : 'a t -> 'b t -> ('a * 'b) t

(** [triple a b c] consumes three elements sequentially using [a], [b] and then
    [c] to parse. *)
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** [maybe t] is a short-hand for:

    {[
      (let+ x = t in
       Some x)
      <|> return None
    ]} *)
val maybe : 'a t -> 'a option t

(** Consume the next element as a duration, requiring 's', 'm' or 'h' suffix. *)
val duration : int t

(** Consume the next element as a number of bytes, requiring 'B', 'KB', 'MB' or
    'GB' suffix. *)
val bytes_unit : int64 t

(** Unparsed next element of the input. *)
val raw : Ast.t t

(** Inspect the next element of the input without consuming it. *)
val peek : Ast.t option t

(** Same as [peek] but fail if the end of input is reached. *)
val peek_exn : Ast.t t

(** Consume and ignore the next element of the input. *)
val junk : unit t

(** Consume and ignore all the remaining input. *)
val junk_everything : (unit, _) parser

(** [plain_string f] expects the next element of the input to be a plain string,
    i.e. either an atom or a quoted string, but not a template nor a list. *)
val plain_string : (loc:Loc.t -> string -> 'a) -> 'a t

(** A valid filename, i.e. a string other than "." or "..". *)
val filename : Filename.t t

(** An extension: a string not starting with ".". The value returned by the
    parser is prefixed with ".". *)
val extension : string t

(** A relative filename. *)
val relative_file : string t

(** [fix f] is the fixpoint of the endofunction of parsers [f]. *)
val fix : ('a t -> 'a t) -> 'a t

(** [located t] returns a parser that returns the result of [t] and the location
    of the parsed element. *)
val located : ('a, 'k) parser -> (Loc.t * 'a, 'k) parser

(** [enum l] parses an atom that is one of the strings in [l] and returns the
    associated value. *)
val enum : (string * 'a) list -> 'a t

(** [enum' l] is like [enum] but the values are instead further parsers. *)
val enum' : (string * 'a t) list -> 'a t

(** Parser that parse a S-expression of the form
    [(<atom> <s-exp1> <s-exp2> ...)] or [<atom>]. [<atom>] is looked up in the
    list and the remaining s-expressions are parsed using the corresponding list
    parser.

    If [force_parens] is [true], then the form [<atom>] is never accepted. The
    default is [false]. *)
val sum : ?force_parens:bool -> (string * 'a t) list -> 'a t

(** Check the result of a list parser, and raise a properly located error in
    case of failure. *)
val map_validate
  :  ('a, 'k) parser
  -> f:('a -> ('b, User_message.t) Result.t)
  -> ('b, 'k) parser

(** {3 Parsing record fields} *)

(** [field name ?default ?on_dup t] parses a field named [name] using the parser
    [t].

    - [default] is used for the parsed value if the field does not occur. An
      error is raised if no default is set instead.

    - [on_dup] is a hook that is called when a field is duplicated allowing for
      a custom error message. When not provided it defaults to an error. *)
val field
  :  string
  -> ?default:'a
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> 'a t
  -> 'a fields_parser

(** [field_o] is like [field] but the field can be optional. Returns an [option]
    type accordingly. *)
val field_o
  :  string
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> 'a t
  -> 'a option fields_parser

(** [fields_mutually_exclusive] is for mutually exclusive fields. If [default] is
    provided, allow the fields' absence. *)
val fields_mutually_exclusive
  :  ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> ?default:'a
  -> (string * 'a t) list
  -> 'a fields_parser

(** [field_b] tests if the field is present. *)
val field_b
  :  ?check:unit t
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> string
  -> bool fields_parser

(** [field_o_b] differentiates between the field not being present and being set
    to true or false. *)
val field_o_b
  :  ?check:unit t
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> string
  -> bool option fields_parser

(** [multi_field] is a field that can appear multiple times. *)
val multi_field : string -> 'a t -> 'a list fields_parser

(** [leftover_fields] returns the remaining list of unparsed S-expressions. *)
val leftover_fields : Ast.t list fields_parser

(** [leftover_fields_as_sums] is like [leftover_fields] but the remaining
    S-expressions are parsed as sums. *)
val leftover_fields_as_sums : (string * 'a t) list -> 'a list fields_parser
