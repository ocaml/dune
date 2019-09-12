open Stdune

type t

val hash : t -> int

val of_string_exn : loc:Loc.t option -> string -> t

val to_string : t -> string

include Dune_lang.Conv.S with type t := t

module Local : sig
  type t

  val encode : t Dune_lang.Encoder.t

  val decode_loc : (Loc.t * (t, unit) Result.t) Dune_lang.Decoder.t

  val validate : Loc.t * (t, unit) Result.t -> t

  val of_string_exn : string -> t

  val of_string : string -> (t, unit) Result.t

  val to_string : t -> string

  (** Description of valid library banes *)
  val valid_format_doc : User_message.Style.t Pp.t

  val pp_quoted : t Fmt.t

  val pp : t Fmt.t
end

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val pp : t Fmt.t

val pp_quoted : t Fmt.t

val of_local : Loc.t * Local.t -> t

val to_local : t -> (Local.t, unit) Result.t

val split : t -> Package.Name.t * string list

val package_name : t -> Package.Name.t

val root_lib : t -> t

module Map : Map.S with type key = t

module Set : sig
  include Set.S with type elt = t

  val to_string_list : t -> string list
end

val nest : t -> t -> t

val to_dyn : t -> Dyn.t
