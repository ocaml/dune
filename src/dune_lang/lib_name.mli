open Stdune

type t

val hash : t -> int

include Dune_util.Stringlike with type t := t

module Local : sig
  include Dune_util.Stringlike

  (** Description of valid library names *)
  val valid_format_doc : User_message.Style.t Pp.t

  val mangled_path_under_package : t -> string list
end

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val of_local : Loc.t * Local.t -> t
val to_local : Loc.t * t -> (Local.t, User_message.t) result
val to_local_exn : t -> Local.t
val split : t -> Package_name.t * string list
val package_name : t -> Package_name.t
val of_package_name : Package_name.t -> t

type analyze =
  | Public of Package_name.t * string list
  | Private of Package_name.t * Local.t

val analyze : t -> analyze
val mangled : Package_name.t -> Local.t -> t

module Map : Map.S with type key = t

module Set : sig
  include Set.S with type elt = t and type 'a map = 'a Map.t

  val to_string_list : t -> string list
end

val nest : t -> t -> t
