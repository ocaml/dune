open! Stdune

type t =
  | String of string
  | Dir of Path.t
  | Path of Path.t

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val to_dyn : t -> Dyn.t

val to_string : t -> dir:Path.t -> string

val to_path : ?error_loc:Loc.t -> t -> dir:Path.t -> Path.t

(** The value corresponds to a dependency given by the user. It is interpreted
    relative to the build directory given. It is converted to an external path
    if the path is outside the workspace. Otherwise a path in the build
    directory is returned. *)
val to_path_in_build_or_external :
  ?error_loc:Loc.t -> t -> dir:Path.Build.t -> Path.t

module L : sig
  val strings : string list -> t list

  (** [compare_vals ~dir a b] is a more efficient version of:

      {[
        List.compare ~compare:String.compare (to_string ~dir a)
          (to_string ~dir b)
      ]} *)
  val compare_vals : dir:Path.t -> t list -> t list -> Ordering.t

  val to_dyn : t list -> Dyn.t

  val paths : Path.t list -> t list

  val deps_only : t list -> Path.t list

  val dirs : Path.t list -> t list

  val concat : t list -> dir:Path.t -> string

  val to_strings : t list -> dir:Path.t -> string list
end
