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
val to_path_in_build_or_external : ?error_loc:Loc.t -> t -> dir:Path.Build.t -> Path.t

val true_ : t
val false_ : t
val of_bool : bool -> t

module L : sig
  val strings : string list -> t list

  (** [compare_vals ~dir a b] is a more efficient version of:

      {[
        List.compare ~compare:String.compare (to_string ~dir a) (to_string ~dir b)
      ]} *)
  val compare_vals : dir:Path.t -> t list -> t list -> Ordering.t

  val to_dyn : t list -> Dyn.t
  val paths : Path.t list -> t list
  val deps_only : t list -> Path.t list
  val dirs : Path.t list -> t list
  val to_strings : t list -> dir:Path.t -> string list
end

module Deferred_concat : sig
  type value = t

  (** When concatenating multiple values together it can sometimes be useful to
      preserve the paths in the [Path] and [Dir] constructors. This type
      represents a concatenation of [value]s which can be forced into a single
      [value] or string. *)
  type t

  val singleton : value -> t

  (** [parts t] returns a list of [value]s that can be concatenated (without a
      delimeter) to produce the [value] represented by [t]. If the [value] is all
      that's required, consider using [force] instead. Use the [parts] function
      if additional processing is to be done on [value]s prior to
      concatenation. *)
  val parts : t -> value list

  (** Construct a [t] by concatenating [value]s *)
  val concat_values : value list -> sep:string option -> t

  (** Construct a [t] by concatenating [t]s *)
  val concat : t list -> sep:string option -> t

  (** [force t ~dir] returns the [value] represented by [t]. If it's necessary
      to stringify a path, (e.g. if a [Path] or [Dir] [value] is concatenated
      with another value) then the stringified path will be relative to the
      provided [dir] argument. *)
  val force : t -> dir:Path.t -> value

  (** Similar to [force] but stringifies the result *)
  val force_string : t -> dir:Path.t -> string
end
