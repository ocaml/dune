open Import

type t =
  | Workspace of Path.Source.t
  | Build of Path.Build.t

val workspace : Path.Source.t -> t
val build : Path.Build.t -> t
val to_path : t -> Path.t
val to_build_dir : workspace_build_dir:Path.Build.t -> t -> Path.Build.t
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val hash : t -> int
val repr : t Repr.t
val to_dyn : t -> Dyn.t
val relative : t -> string -> t
val relative_fname : t -> Filename.t -> t
val append_local : t -> Path.Local.t -> t
val basename : t -> Filename.t
val parent : t -> t option
val parent_exn : t -> t
val descendant : t -> of_:t -> Path.Local.t option
val is_descendant : t -> of_:t -> bool
val to_string : t -> string
val to_string_maybe_quoted : t -> string
val as_workspace : t -> Path.Source.t option
val as_workspace_exn : t -> Path.Source.t

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
