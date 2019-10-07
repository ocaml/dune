open Stdune

type t

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> Ordering.t

val make : string -> dir:Path.Build.t -> t

(** The following always holds:

    {[ make (name t) ~dir:(dir t) = t ]} *)
val name : t -> string

val dir : t -> Path.Build.t

val stamp_file_dir : t -> Path.Build.t

val to_dyn : t -> Dyn.t

val encode : t Dune_lang.Encoder.t

val pp : t Fmt.t

val of_user_written_path : loc:Loc.t -> Path.t -> t

val fully_qualified_name : t -> Path.Build.t

val default : dir:Path.Build.t -> t

val runtest : dir:Path.Build.t -> t

val install : dir:Path.Build.t -> t

val doc : dir:Path.Build.t -> t

val private_doc : dir:Path.Build.t -> t

val lint : dir:Path.Build.t -> t

val all : dir:Path.Build.t -> t

val check : dir:Path.Build.t -> t

val fmt : dir:Path.Build.t -> t

(** Return the underlying stamp file *)
val stamp_file : t -> Path.Build.t

val find_dir_specified_on_command_line : dir:Path.Source.t -> File_tree.Dir.t

val is_standard : string -> bool

val suffix : string

val alias_dir : Path.Build.t
