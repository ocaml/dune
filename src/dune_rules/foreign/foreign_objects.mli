open Import

(** For the [(extra_objects ...)] field.*)
type t

val empty : t
val is_empty : t -> bool
val decode : t Dune_lang.Decoder.t
val build_paths : t -> ext_obj:Filename.Extension.t -> dir:Path.Build.t -> Path.t list
