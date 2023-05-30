open Import

module Name : sig
  include
    module type of Dune_util.Alias_name with type t = Dune_util.Alias_name.t

  val default : t

  val runtest : t

  val install : t

  val fmt : t

  val all : t

  val parse_local_path : Loc.t * Path.Local.t -> Path.Local.t * t

  include Comparable_intf.S with type key := t
end

type t

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> Ordering.t

val make : Name.t -> dir:Path.Build.t -> t

val register_as_standard : Name.t -> unit

(** The following always holds: [make (name t) ~dir:(dir t) = t] *)
val name : t -> Name.t

val dir : t -> Path.Build.t

val to_dyn : t -> Dyn.t

val encode : t Dune_sexp.Encoder.t

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

val is_standard : Name.t -> bool

val describe : ?loc:Loc.t -> t -> _ Pp.t
