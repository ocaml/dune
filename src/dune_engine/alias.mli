open Stdune

module Name : sig
  type t

  val decode : t Dune_lang.Decoder.t

  val of_string : string -> t

  val parse_string_exn : Loc.t * string -> t

  val to_string : t -> string

  val to_dyn : t -> Dyn.t

  val default : t

  val runtest : t

  val install : t

  val all : t

  val parse_local_path : Loc.t * Path.Local.t -> Path.Local.t * t

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t
end

type t

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> Ordering.t

val make : Name.t -> dir:Path.Build.t -> t

(** The following always holds: [make (name t) ~dir:(dir t) = t] *)
val name : t -> Name.t

val dir : t -> Path.Build.t

val stamp_file_dir : t -> Path.Build.t

val to_dyn : t -> Dyn.t

val encode : t Dune_lang.Encoder.t

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

val is_standard : Name.t -> bool

val suffix : string
