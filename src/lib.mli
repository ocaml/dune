open Import

type t =
  | Internal of Path.t * Jbuild_types.Library.t
  | External of Findlib.package

module Set : Set.S with type elt := t

val deps : t -> string list

val include_flags : t list -> _ Arg_spec.t

val link_flags : t list -> mode:Mode.t -> _ Arg_spec.t

val archive_files : t list -> mode:Mode.t -> Path.t list

(** [public_name] if present, [name] if not *)
val best_name : t -> string

val describe : t -> string

val ppx_runtime_libraries : t list -> String_set.t

