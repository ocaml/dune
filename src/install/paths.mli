open Import

type 'a t

val make
  :  relative:('a -> string -> 'a)
  -> package:Package_name.t
  -> roots:'a Roots.t
  -> 'a t

val get : 'a t -> Section.t -> 'a
val get_local_location : Context_name.t -> Section.t -> Package_name.t -> Path.t
val map : 'a t -> f:('a -> 'b) -> 'b t
