open Import

module Local : sig
  type t

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  val equal : t -> t -> bool
  val make : loc:Loc.t -> src_dir:Path.Source.t -> Lib_name.t -> t
  val name : t -> Lib_name.t
  val loc : t -> Loc.t
  val to_dyn : t -> Dyn.t
end

type t =
  | External of (Loc.t * Lib_name.t)
  | Local of Local.t

module Map : Map.S with type key = t
module Set : Set.S with type elt = t

val to_local_exn : t -> Local.t
val name : t -> Lib_name.t
val loc : t -> Loc.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
