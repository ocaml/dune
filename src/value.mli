open Stdune

type t =
  | String of string
  | Path of Path.t

val to_string : t -> dir:Path.t -> string

val to_strings : t list -> dir:Path.t -> string list

val to_path : ?error_loc:Loc.t -> t -> dir:Path.t -> Path.t

val strings : string list -> t list

val paths : Path.t list -> t list

val paths_only : t list -> Path.t list

val concat : t list -> dir:Path.t -> string
