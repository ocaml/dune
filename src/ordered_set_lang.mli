(** [Ordered_set_lang.t] is a sexp-based representation for an ordered list of strings,
    with some set like operations. *)

open Import

type t
val t : t Sexp.Of_sexp.t

(** Return the location of the set. [loc standard] returns [None] *)
val loc : t -> Loc.t option

(** Value parsed from elements in the DSL *)
module type Value = sig
  type t
  val name : t -> string
end

module Make(Value : Value) : sig
  (** Evaluate an ordered set. [standard] is the interpretation of [:standard] inside the
      DSL. *)
  val eval
    :  t
    -> parse:(loc:Loc.t -> string -> Value.t)
    -> standard:Value.t list
    -> Value.t list

  (** Same as [eval] but the result is unordered *)
  val eval_unordered
    :  t
    -> parse:(loc:Loc.t -> string -> Value.t)
    -> standard:Value.t String_map.t
    -> Value.t String_map.t
end

val standard : t
val is_standard : t -> bool

module Unexpanded : sig
  type expanded = t
  type t
  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t
  val standard : t

  val field : ?default:t -> string -> t Sexp.Of_sexp.record_parser

  (** List of files needed to expand this set *)
  val files : t -> f:(String_with_vars.t -> string) -> String_set.t

  (** Expand [t] using with the given file contents. [file_contents] is a map from
      filenames to their parsed contents. Every [(:include fn)] in [t] is replaced by
      [Map.find files_contents fn]. Every element is converted to a string using [f]. *)
  val expand
    :  t
    -> files_contents:Sexp.Ast.t String_map.t
    -> f:(String_with_vars.t -> string)
    -> expanded
end with type expanded := t
