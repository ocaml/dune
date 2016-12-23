(** [Ordered_set_lang.t] is a sexp-based representation for an ordered list of strings,
    with some set like operations. *)

open Import

type t
val t : Sexp.t -> t

val eval_with_standard : t -> standard:string list -> string list
val standard : t
val is_standard : t -> bool

(** Map non-variable atoms *)
val map : t -> f:(string -> string) -> t

module Unexpanded : sig
  type expanded = t
  type t
  val t : Sexp.t -> t
  val standard : t

  (** List of files needed to expand this set *)
  val files : t -> String_set.t

  (** Expand [t] using with the given file contents. [file_contents] is a map from
      filenames to their parsed contents. Every [(:include fn)] in [t] is replaced by
      [Map.find files_contents fn]. *)
  val expand : t -> files_contents:Sexp.t String_map.t -> expanded
end with type expanded := t
