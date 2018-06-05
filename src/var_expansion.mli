open Stdune

type t =
  | Paths   of Path.t list
  | Strings of string list

val to_string : Path.t -> t -> string
(** [to_string dir v] convert the variable expansion to a string.
    If it is a path, the corresponding string will be relative to
    [dir]. *)

val path_of_string : Path.t -> string -> Path.t

val to_strings : Path.t -> t -> string list

val to_path : Path.t -> t -> Path.t

module Expand : String_with_vars.Expand_intf
  with type expansion = t and type context = Path.t

(** Specialized expansion that produce only a single value *)
module Single : sig
  val path
    :  dir:Path.t
    -> String_with_vars.t
    -> f:(Loc.t -> string -> t option)
    -> Path.t

  val string
    :  dir:Path.t
    -> String_with_vars.t
    -> f:(Loc.t -> string -> t option)
    -> string
end
