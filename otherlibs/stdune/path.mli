(** Representation of paths *)

(** The aim of this module is to provide a solid basis to reason about file and
    directory paths inside the Dune code base. What it is not is a complete API
    for paths management that handles all the aspects of file system paths. It
    simply exposes a high-level and portable API that covers the needs of Dune.

    {1 Model of the file system}

    {2 Local paths}

    Dune sees the file system as two parts. The first part is composed of the
    source tree and the build directory. In this part, Dune doesn't know about
    symlinks and has a fully expanded view of the file system. This means that
    if the user has a symlink `src/foo` pointing to `bar`, then `src/foo/x` and
    `bar/x` are seen as two different paths.

    A path in this world is called a local path and is simply a sequence of path
    components. A path component being a string other than "." or ".." and not
    containing the path separator character ('/').

    Such a path can be rooted at the source tree root, the build directory or an
    unspecified root. All these paths are represented by values of type
    ['a Path.Local_gen.t] where ['a] denotes the root of the path.

    {2 External paths}

    The second part is the "external world". It is all the paths that live
    outside of the workspace and build directory. To be on the safe side Dune
    makes no assumption does nothing clever with these paths.

    External paths are represented as [Path.External.t] values.

    {1 The Path.t type}

    The [Path.t] type represents all possible paths, i.e. both local and
    external paths. *)

(** Relative path relative to the root tracked by the type system.

    Represented as: either the root, or a '/' separated list of components other
    that ".", ".." and not containing a '/'. *)
module Local_gen : Path_intf.Local_gen

module Unspecified : sig
  type w = Path_intf.Unspecified.w
end

(** Relative path with unspecified root.

    Either root, or a '/' separated list of components other that ".", ".." and
    not containing a '/'. *)
module Local : sig
  type w = Unspecified.w

  type t = w Local_gen.t

  include Path_intf.S with type t := t

  val root : t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  val relative : ?error_loc:Loc0.t -> t -> string -> t

  val split_first_component : t -> (string * t) option

  val explode : t -> string list
end

module External : sig
  include Path_intf.S

  val initial_cwd : t

  val cwd : unit -> t

  val relative : t -> string -> t

  val mkdir_p : ?perms:int -> t -> unit
end

(** In the source section of the current workspace. *)
module Source : sig
  type w

  type t = w Local_gen.t

  include Path_intf.S with type t := t

  val root : t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  val of_local : Local.t -> t

  (** [relative dir s] if s can be ".." it could escape the working directory.
      {!Path.relative} should be used instead. *)
  val relative : ?error_loc:Loc0.t -> t -> string -> t

  val split_first_component : t -> (string * Local.t) option

  val explode : t -> string list

  (** [Source.t] does not statically forbid overlap with build directory, even
      though having such paths is almost always an error. *)
  val is_in_build_dir : t -> bool

  val descendant : t -> of_:t -> t option

  val to_local : t -> Local.t
end

module Permissions : sig
  type t

  (** Execute permissions. *)
  val execute : t

  (** Write permissions. *)
  val write : t

  (** Add permissions to a given mask for the current user. *)
  val add : t -> int -> int

  (** Test permissions of a given mask for the current user. *)
  val test : t -> int -> bool

  (** Remove permissions from a given mask for all users. *)
  val remove : t -> int -> int
end

module Outside_build_dir : sig
  type t =
    | External of External.t
    | In_source_dir of Source.t

  val hash : t -> int

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t

  val of_string : string -> t

  val to_string : t -> string

  val to_string_maybe_quoted : t -> string

  val parent : t -> t option

  module Table : Hashtbl.S with type key = t
end

module Build : sig
  type w

  type t = w Local_gen.t

  include Path_intf.S with type t := t

  val root : t

  val append_source : t -> Source.t -> t

  val append_local : t -> Local.t -> t

  (** [append x y] is [append_local x (local y)] *)
  val append : t -> t -> t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  val relative : ?error_loc:Loc0.t -> t -> string -> t

  val split_first_component : t -> (string * Local.t) option

  val explode : t -> string list

  val local : t -> Local.t

  val drop_build_context : t -> Source.t option

  val drop_build_context_exn : t -> Source.t

  val drop_build_context_maybe_sandboxed_exn : t -> Source.t

  (** [Source.t] here is a lie in some cases: consider when the context name
      happens to be ["install"] or [".alias"]. *)
  val extract_build_context : t -> (string * Source.t) option

  val extract_build_context_exn : t -> string * Source.t

  val extract_build_context_dir : t -> (t * Source.t) option

  val extract_build_context_dir_exn : t -> t * Source.t

  (** This function does the same as [extract_build_context], but has a
      "righter" type. *)
  val extract_first_component : t -> (string * Local.t) option

  (** Set the build directory. Can only be called once and must be done before
      paths are converted to strings elsewhere. *)
  val set_build_dir : Outside_build_dir.t -> unit

  val split_sandbox_root : t -> t option * t

  val of_local : Local.t -> t

  (** Set permissions for a given path. You can use the [Permissions] module if
      you need to modify existing permissions in a non-trivial way. *)
  val chmod : t -> mode:int -> unit

  val lstat : t -> Unix.stats

  val unlink_no_err : t -> unit
end

type t = private
  | External of External.t
  | In_source_tree of Source.t
  | In_build_dir of Build.t

include Path_intf.S with type t := t

val as_outside_build_dir_exn : t -> Outside_build_dir.t

val destruct_build_dir :
  t -> [ `Inside of Build.t | `Outside of Outside_build_dir.t ]

val outside_build_dir : Outside_build_dir.t -> t

val hash : t -> int

(** [to_string_maybe_quoted t] is [maybe_quoted (to_string t)] *)
val to_string_maybe_quoted : t -> string

val root : t

val external_ : External.t -> t

val is_root : t -> bool

val is_managed : t -> bool

val relative : ?error_loc:Loc0.t -> t -> string -> t

(** [relative_to_source_in_build ~dir s] compute the path [s] relative to the
    source directory corresponding to [dir] *)
val relative_to_source_in_build_or_external :
  ?error_loc:Loc0.t -> dir:Build.t -> string -> t

(** Create an external path. If the argument is relative, assume it is relative
    to the initial directory dune was launched in. *)
val of_filename_relative_to_initial_cwd : string -> t

(** Convert a path to an absolute filename. Must be called after the workspace
    root has been set. [root] is the root directory of local paths *)
val to_absolute_filename : t -> string

val pp : t -> _ Pp.t

(** Reach a given path [from] a directory. For example, let [p] be a path to the
    file [some/dir/file] and [d] be a path to the directory [some/another/dir].
    Then [reach p ~from:d] evaluates to [../../dir/file]. *)
val reach : t -> from:t -> string

(** [from] defaults to [Path.root] *)
val reach_for_running : ?from:t -> t -> string

val descendant : t -> of_:t -> t option

val is_descendant : t -> of_:t -> bool

val append_local : t -> Local.t -> t

val append_source : t -> Source.t -> t

val extend_basename : t -> suffix:string -> t

(** Extract the build context from a path. For instance, representing paths as
    strings:

    {[
      extract_build_context "_build/blah/foo/bar" = Some ("blah", "foo/bar")
    ]}

    It doesn't work correctly (doesn't return a sensible source path) for build
    directories that are not build contexts, e.g. "_build/install" and
    "_build/.aliases". *)
val extract_build_context : t -> (string * Source.t) option

val extract_build_context_exn : t -> string * Source.t

val extract_build_dir_first_component : t -> (string * Local.t) option

(** Same as [extract_build_context] but return the build context as a path:

    {[
      extract_build_context "_build/blah/foo/bar"
      = Some ("_build/blah", "foo/bar")
    ]} *)
val extract_build_context_dir : t -> (t * Source.t) option

val extract_build_context_dir_maybe_sandboxed : t -> (t * Source.t) option

val extract_build_context_dir_exn : t -> t * Source.t

(** Drop the "_build/blah" prefix *)
val drop_build_context : t -> Source.t option

val drop_build_context_exn : t -> Source.t

(** Drop the "_build/blah" prefix if present, return [t] otherwise *)
val drop_optional_build_context : t -> t

val drop_optional_build_context_maybe_sandboxed : t -> t

val drop_optional_sandbox_root : t -> t

(** Drop the "_build/blah" prefix if present, return [t] if it's a source file,
    otherwise fail. *)
val drop_optional_build_context_src_exn : t -> Source.t

val explode : t -> string list option

val explode_exn : t -> string list

(** The build directory *)
val build_dir : t

(** [is_in_build_dir t = is_descendant t ~of:build_dir] *)
val is_in_build_dir : t -> bool

(** [is_in_source_tree t = is_managed t && not (is_in_build_dir t)] *)
val is_in_source_tree : t -> bool

val as_in_source_tree : t -> Source.t option

val as_in_source_tree_exn : t -> Source.t

val as_in_build_dir : t -> Build.t option

val as_in_build_dir_exn : t -> Build.t

val as_external : t -> External.t option

(** [is_strict_descendant_of_build_dir t = is_in_build_dir t && t <> build_dir] *)
val is_strict_descendant_of_build_dir : t -> bool

(** Split after the first component if [t] is local *)
val split_first_component : t -> (string * t) option

val insert_after_build_dir_exn : t -> string -> t

val exists : t -> bool

val readdir_unsorted :
  t -> (string list, Dune_filesystem_stubs.Unix_error.Detailed.t) Result.t

val readdir_unsorted_with_kinds :
     t
  -> ( (string * Unix.file_kind) list
     , Dune_filesystem_stubs.Unix_error.Detailed.t )
     Result.t

val is_dir_sep : char -> bool

val is_directory : t -> bool

val is_directory_with_error : t -> (bool, string) Result.t

val is_file : t -> bool

val rmdir : t -> unit

val unlink : t -> unit

val unlink_no_err : t -> unit

val link : t -> t -> unit

(** If the path does not exist, this function is a no-op. *)
val rm_rf : ?allow_external:bool -> t -> unit

(** [clear_dir t] deletes all the contents of directory [t] without removing [t]
    itself. *)
val clear_dir : t -> Fpath.clear_dir_result

val mkdir_p : ?perms:int -> t -> unit

val touch : ?create:bool -> t -> unit

val build_dir_exists : unit -> bool

val ensure_build_dir_exists : unit -> unit

val source : Source.t -> t

val build : Build.t -> t

(** paths guaranteed to be in the source directory *)
val in_source : string -> t

val of_local : Local.t -> t

(** Set the workspace root. Can only be called once and the path must be
    absolute *)
val set_root : External.t -> unit

module L : sig
  val relative : t -> string list -> t
end

(** Return the "local part" of a path. For local paths (in build directory or
    source tree), this returns the path itself. For external paths, it returns a
    path that is relative to the current directory. For example, the local part
    of [/a/b] is [./a/b]. *)
val local_part : t -> Local.t

val stat :
  t -> (Unix.stats, Dune_filesystem_stubs.Unix_error.Detailed.t) Result.t

val stat_exn : t -> Unix.stats

val lstat :
  t -> (Unix.stats, Dune_filesystem_stubs.Unix_error.Detailed.t) Result.t

val lstat_exn : t -> Unix.stats

(* it would be nice to call this [Set.of_source_paths], but it's annoying to
   change the [Set] signature because then we don't comply with [Path_intf.S] *)
val set_of_source_paths : Source.Set.t -> Set.t

val set_of_build_paths_list : Build.t list -> Set.t

val set_of_external_paths : External.Set.t -> Set.t

(** Rename a file. [rename oldpath newpath] renames the file called [oldpath] to
    [newpath], moving it between directories if needed. If [newpath] already
    exists, its contents will be replaced with those of [oldpath]. *)
val rename : t -> t -> unit

(** Set permissions for a given path. You can use the [Permissions] module if
    you need to modify existing permissions in a non-trivial way. *)
val chmod : t -> mode:int -> unit

(** Attempts to resolve a symlink. Returns [None] if the path isn't a symlink *)
val follow_symlink : t -> (t, Fpath.follow_symlink_error) result

module Expert : sig
  (** Attempt to convert external paths to source/build paths. Don't use this
      function unless strictly necessary. It's not completely reliable and we
      only use it out of necessity to work with file watchers that insist on
      spitting absolute paths *)
  val try_localize_external : t -> t
end
