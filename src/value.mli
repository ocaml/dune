open Stdune

type t =
  | String of string
  | Dir of Path.t
  | Path of Path.t

val to_string : t -> dir:Path.t -> string

val to_path : ?error_loc:Loc.t -> t -> dir:Path.t -> Path.t

val compare : t -> t -> Ordering.t

val compare_as_string : dir:Path.t -> t -> t -> Ordering.t

module L : sig
  val strings : string list -> t list

  val paths : Path.t list -> t list

  val deps_only : t list -> Path.t list

  val dirs : Path.t list -> t list

  val concat : t list -> dir:Path.t -> string

  val to_strings : t list -> dir:Path.t -> string list
end
