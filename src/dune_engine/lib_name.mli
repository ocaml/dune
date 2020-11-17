open Import

type t

val hash : t -> int

include Stringlike_intf.S with type t := t

module Local : sig
  include Stringlike_intf.S

  (** Description of valid library names *)
  val valid_format_doc : User_message.Style.t Pp.t

  val mangled_path_under_package : t -> string list
end

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val of_local : Loc.t * Local.t -> t

val to_local : Loc.t * t -> (Local.t, User_message.t) result

val to_local_exn : t -> Local.t

val split : t -> Package.Name.t * string list

val package_name : t -> Package.Name.t

val of_package_name : Package.Name.t -> t

type analyze =
  | Public of Package.Name.t * string list
  | Private of Package.Name.t * Local.t

val analyze : t -> analyze

val mangled : Package.Name.t -> Local.t -> t

module Map : Map.S with type key = t

module Set : sig
  include Set.S with type elt = t

  val to_string_list : t -> string list
end

val nest : t -> t -> t
