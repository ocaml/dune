open Import

include module type of struct include Usexp end with module Loc := Usexp.Loc

val code_error : string -> (string * t) list -> _

val load : fname:string -> mode:'a Parser.Mode.t -> 'a
val load_many_as_one : fname:string -> Ast.t

type sexps_or_ocaml_script =
  | Sexps of Ast.t list
  | Ocaml_script

val load_many_or_ocaml_script : string -> sexps_or_ocaml_script

module type Combinators = sig
  type 'a t
  val unit       : unit                      t

  val string     : string                    t
  (** Convert an [Atom] or a [Quoted_string] from/to a string. *)

  val int        : int                       t
  val float      : float                     t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val triple     : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list       : 'a t -> 'a list           t
  val array      : 'a t -> 'a array          t
  val option     : 'a t -> 'a option         t

  val string_set : String_set.t            t
  (** [atom_set] is a conversion to/from a set of strings representing atoms. *)

  val string_map : 'a t -> 'a String_map.t   t
  (** [atom_map conv]: given a conversion [conv] to/from ['a], returns
     a conversion to/from a map where the keys are atoms and the
     values are of type ['a]. *)

  val string_hashtbl : 'a t -> (string, 'a) Hashtbl.t t
  (** [atom_hashtbl conv] is similar to [atom_map] for hash tables. *)
end

module To_sexp : sig
  type sexp = t
  include Combinators with type 'a t = 'a -> t

  val record : (string * sexp) list -> sexp
end with type sexp := t

module Of_sexp : sig
  type ast = Ast.t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | List of Loc.t * ast list

  include Combinators with type 'a t = Ast.t -> 'a

  val of_sexp_error  : Ast.t -> string -> _
  val of_sexp_errorf : Ast.t -> ('a, unit, string, 'b) format4 -> 'a

  val located : 'a t -> (Loc.t * 'a) t

  (* Record parsing monad *)
  type 'a record_parser
  val return : 'a -> 'a record_parser
  val ( >>= ) : 'a record_parser -> ('a -> 'b record_parser) -> 'b record_parser

  (** Return the location of the record being parsed *)
  val record_loc : Loc.t record_parser

  module Short_syntax : sig
    type 'a t =
      | Not_allowed
      | This    of 'a
      | Located of (Loc.t -> 'a)
  end

  val field
    :  string
    -> ?short:'a Short_syntax.t
    -> ?default:'a
    -> 'a t
    -> 'a record_parser
  val field_o
    :  string
    -> ?short:'a Short_syntax.t
    -> 'a t
    -> 'a option record_parser
  val field_b : string -> bool record_parser

  val map_validate : 'a record_parser -> f:('a -> ('b, string) result) -> 'b record_parser

  val ignore_fields : string list -> unit record_parser

  val record : 'a record_parser -> 'a t

  module Constructor_spec : sig
    type 'a t
  end

  module Constructor_args_spec : sig
    type ('a, 'b) t
  end

  val nil : ('a, 'a) Constructor_args_spec.t
  val ( @> )
    :  'a t
    -> ('b, 'c) Constructor_args_spec.t
    -> ('a -> 'b, 'c) Constructor_args_spec.t

  val cstr : string -> ('a, 'b) Constructor_args_spec.t -> 'a -> 'b Constructor_spec.t
  val cstr_rest
    :  string
    -> ('a, 'b list -> 'c) Constructor_args_spec.t
    -> 'b t
    -> 'a
    -> 'c Constructor_spec.t

  val cstr_record : string -> 'a record_parser -> 'a Constructor_spec.t

  val cstr_loc
    :  string
    -> ('a, 'b) Constructor_args_spec.t
    -> (Loc.t -> 'a)
    -> 'b Constructor_spec.t

  val cstr_rest_loc
    :  string
    -> ('a, 'b list -> 'c) Constructor_args_spec.t
    -> 'b t
    -> (Loc.t -> 'a)
    -> 'c Constructor_spec.t

  val sum
    :  'a Constructor_spec.t list
    -> 'a t

  val enum : (string * 'a) list -> 'a t
end
