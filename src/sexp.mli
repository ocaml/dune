open Import

type t =
  | Atom of string
  | List of t list

module Ast : sig
  type sexp = t
  type t =
    | Atom of Loc.t * string
    | List of Loc.t * t list

  val loc : t -> Loc.t

  val remove_locs : t -> sexp
  val to_string : t -> string
end with type sexp := t

val add_loc : t -> loc:Loc.t -> Ast.t

val code_error : string -> (string * t) list -> _

val to_string : t -> string

module type Combinators = sig
  type 'a t
  val unit       : unit                      t
  val string     : string                    t
  val int        : int                       t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val list       : 'a t -> 'a list           t
  val option     : 'a t -> 'a option         t
  val string_set : String_set.t              t
  val string_map : 'a t -> 'a String_map.t   t
end

module To_sexp : Combinators with type 'a t = 'a -> t

module Of_sexp : sig
  type ast = Ast.t =
    | Atom of Loc.t * string
    | List of Loc.t * ast list

  include Combinators with type 'a t = Ast.t -> 'a

  val of_sexp_error  : Ast.t -> string -> _
  val of_sexp_errorf : Ast.t -> ('a, unit, string, 'b) format4 -> 'a

  (* Record parsing monad *)
  type 'a record_parser
  val return : 'a -> 'a record_parser
  val ( >>= ) : 'a record_parser -> ('a -> 'b record_parser) -> 'b record_parser

  val field   : string -> ?default:'a -> 'a t -> 'a record_parser
  val field_o : string -> 'a t -> 'a option record_parser
  val field_b : string -> bool record_parser

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

  val sum
    :  'a Constructor_spec.t list
    -> 'a t

  val enum : (string * 'a) list -> 'a t
end
