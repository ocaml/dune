(** [Ordered_set_lang.t] is a sexp-based representation for an ordered list of strings,
    with some set like operations. *)

open Import

type t
val t : t Sexp.Of_sexp.t

val eval_with_standard : t -> standard:string list -> string list
val standard : t
val is_standard : t -> bool
val append : t -> t -> t

(** Map non-variable atoms *)
val map : t -> f:(string -> string) -> t

module Unexpanded : sig
  type expanded = t
  type t
  val t : t Sexp.Of_sexp.t
  val standard : t

  val append : t -> t -> t

  (** List of files needed to expand this set *)
  val files : t -> f:(String_with_vars.t -> string) -> String_set.t

  (** Expand [t] using with the given file contents. [file_contents] is a map from
      filenames to their parsed contents. Every [(:include fn)] in [t] is replaced by
      [Map.find files_contents fn]. Every element is converted to a string using [f]. *)
  val expand : t -> files_contents:Sexp.Ast.t String_map.t -> f:(String_with_vars.t -> string) -> expanded
end with type expanded := t
