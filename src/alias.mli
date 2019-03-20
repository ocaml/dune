open Stdune

type t

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> Ordering.t

val make : string -> dir:Path.t -> t

(** The following always holds:

    {[
      make (name t) ~dir:(dir t) = t
    ]}
*)
val name : t -> string
val dir : t -> Path.t

val to_dyn : t -> Dyn.t

val to_sexp : t -> Sexp.t

val encode : t Dune_lang.Encoder.t

val pp : t Fmt.t

val of_user_written_path : loc:Loc.t -> Path.t -> t

val fully_qualified_name : t -> Path.t

val default     : dir:Path.t -> t
val runtest     : dir:Path.t -> t
val install     : dir:Path.t -> t
val doc         : dir:Path.t -> t
val private_doc : dir:Path.t -> t
val lint        : dir:Path.t -> t
val all         : dir:Path.t -> t
val check       : dir:Path.t -> t
val fmt         : dir:Path.t -> t

(** Return the underlying stamp file *)
val stamp_file : t -> Path.t

val find_dir_specified_on_command_line
  :  dir:Path.t
  -> file_tree:File_tree.t
  -> File_tree.Dir.t

val is_standard : string -> bool
val suffix : string
