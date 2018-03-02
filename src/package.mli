(** Information about a package defined in the workspace *)

module Name : sig
  type t = private string

  val of_string : string -> t

  val opam_fn : t -> string

  module Map : Stdune.Map.S with type key = t
  module Set : Stdune.Set.S with type elt = t

  val pp : Format.formatter -> t -> unit
end

type t =
  { name                   : Name.t
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

val opam_file : t -> Path.t
