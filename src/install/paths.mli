open Import

type t

val make : package:Package_name.t -> roots:Path.t Roots.t -> t
val get : t -> Section.t -> Path.t
val get_local_location : Context_name.t -> Section.t -> Package_name.t -> Path.t
