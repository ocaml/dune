(** Relative path with unspecified root *)
module Relative : sig
  include Path_intf.S

  (** [root] refers to empty relative path, so whatever the path is interpreted
      relative to. *)
  val root : t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  val relative : ?error_loc:Loc0.t -> t -> string -> t
  val split_first_component : t -> (string * t) option
  val explode : t -> string list
end

(** In the current workspace (anything under the current project root) *)
module Local : sig
  include Path_intf.S
  val root : t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  val relative : ?error_loc:Loc0.t -> t -> string -> t
  val split_first_component : t -> (string * Relative.t) option
  val explode : t -> string list
end

(** In the source section of the current workspace. *)
module Source : sig
  include Path_intf.S
  val root : t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  val of_relative : Relative.t -> t
  val relative : ?error_loc:Loc0.t -> t -> string -> t
  val split_first_component : t -> (string * Relative.t) option
  val explode : t -> string list

  (** [Source.t] does not statically forbid overlap with build directory,
      even though having such paths is almost always an error. *)
  val is_in_build_dir : t -> bool

  val to_local : t -> Local.t
end

module Build : sig
  include Path_intf.S
  val root : t

  val append_source : t -> Source.t -> t

  val append_relative : t -> Relative.t -> t

  val append_local : t -> Local.t -> t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  val relative : ?error_loc:Loc0.t -> t -> string -> t
  val split_first_component : t -> (string * Relative.t) option
  val explode : t -> string list

end

(** In the outside world *)
module External : sig
  include Path_intf.S

  val initial_cwd : t

  val cwd : unit -> t

  val relative : t -> string -> t

  val mkdir_p : t -> unit
end

module Kind : sig
  type t = private
    | External of External.t
    | Local    of Local.t

  val of_string : string -> t
end

include Path_intf.S

val hash : t -> int

module Table : Hashtbl.S with type key = t

(** [to_string_maybe_quoted t] is [maybe_quoted (to_string t)] *)
val to_string_maybe_quoted : t -> string

val kind : t -> Kind.t

val root : t
val is_root : t -> bool

val is_managed : t -> bool

val relative : ?error_loc:Loc0.t -> t -> string -> t

(** Create an external path. If the argument is relative, assume it is relative
    to the initial directory dune was launched in. *)
val of_filename_relative_to_initial_cwd : string -> t

(** Convert a path to an absolute filename. Must be called after the workspace
    root has been set. [root] is the root directory of local paths *)
val to_absolute_filename : t -> string

val reach : t -> from:t -> string

(** [from] defaults to [Path.root] *)
val reach_for_running : ?from:t -> t -> string

val descendant : t -> of_:t -> t option
val is_descendant : t -> of_:t -> bool

val append : t -> t -> t
val append_relative : t -> Relative.t -> t
val append_local : t -> Local.t -> t
val append_source : t -> Source.t -> t

val extend_basename : t -> suffix:string -> t

(** Extract the build context from a path. For instance, representing paths as strings:

    {[
      extract_build_context "_build/blah/foo/bar" = Some ("blah", "foo/bar")
    ]}

    It doesn't work correctly (doesn't return a sensible source path) for build
    directories that are not build contexts, e.g. "_build/install" and "_build/.aliases".
*)
val extract_build_context     : t -> (string * Source.t) option
val extract_build_context_exn : t -> (string * Source.t)

val extract_build_dir_first_component     : t -> (string * Relative.t) option

(** Same as [extract_build_context] but return the build context as a path:

    {[
      extract_build_context "_build/blah/foo/bar" = Some ("_build/blah", "foo/bar")
    ]}
*)
val extract_build_context_dir     : t -> (t * Source.t) option
val extract_build_context_dir_exn : t -> (t * Source.t)

(** Drop the "_build/blah" prefix *)
val drop_build_context : t -> Source.t option
val drop_build_context_exn : t -> Source.t

(** Drop the "_build/blah" prefix if present, return [t] otherwise *)
val drop_optional_build_context : t -> t

(** Drop the "_build/blah" prefix if present, return [t] if it's a source file,
    otherwise fail. *)
val drop_optional_build_context_src_exn : t -> Source.t

(** Transform managed paths so that they are descedant of
    [sandbox_dir]. *)
val sandbox_managed_paths : sandbox_dir:Build.t -> t -> t

val explode : t -> string list option
val explode_exn : t -> string list

(** The build directory *)
val build_dir : t

(** [is_in_build_dir t = is_descendant t ~of:build_dir] *)
val is_in_build_dir : t -> bool

(** [is_in_build_dir t = is_managed t && not (is_in_build_dir t)] *)
val is_in_source_tree : t -> bool
val as_in_source_tree : t -> Source.t option
val as_in_build_dir : t -> Build.t option
val as_in_build_dir_exn : t -> Build.t

val is_alias_stamp_file : t -> bool

(** [is_strict_descendant_of_build_dir t = is_in_build_dir t && t <>
    build_dir] *)
val is_strict_descendant_of_build_dir : t -> bool

(**  Split after the first component if [t] is local *)
val split_first_component : t -> (string * t) option

val insert_after_build_dir_exn : t -> string -> t

val exists : t -> bool
val readdir_unsorted : t -> (string list, Unix.error) Result.t
val is_directory : t -> bool
val is_file : t -> bool
val rmdir : t -> unit
val unlink : t -> unit
val unlink_no_err : t -> unit
val rm_rf : t -> unit
val mkdir_p : t -> unit

val pp_in_source : Format.formatter -> t -> unit
val pp_debug : Format.formatter -> t -> unit

val build_dir_exists : unit -> bool

val ensure_build_dir_exists : unit -> unit

(** set the build directory. Can only be called once and must be done before
    paths are converted to strings elsewhere. *)
val set_build_dir : Kind.t -> unit

val source : Source.t -> t
val build : Build.t -> t

(** paths guaranteed to be in the source directory *)
val in_source : string -> t

val of_local : Local.t -> t

(** Set the workspace root. Can only be called once and the path must be
    absolute *)
val set_root : External.t -> unit

(** Internal use only *)
module Internal : sig
  val raw_kind : t -> Kind.t
end

module L : sig
  val relative : t -> string list -> t
end

(** Return the "local part" of a path.
    For local paths (in build directory or source tree),
    this returns the path itself.
    For external paths, it returns a path that is relative to the current
    directory. For example, the local part of [/a/b] is [./a/b]. *)
val local_part : t -> Relative.t

val stat : t -> Unix.stats

(* it would be nice to call this [Set.of_source_paths], but it's annoying
   to change the [Set] signature because then we don't comply with [Path_intf.S]
*)
val set_of_source_paths : Source.Set.t -> Set.t
