(** Functions on paths that are represented as strings *)

(** No parent directory, use [mkdir_p] if you want to create it too. *)
type mkdir_result =
  [ `Already_exists (** The directory already exists. No action was taken. *)
  | `Created (** The directory was created. *)
  | `Missing_parent_directory
  ]

val mkdir : ?perms:int -> string -> mkdir_result

type mkdir_p_result =
  [ `Already_exists (** The directory already exists. No action was taken. *)
  | `Created (** The directory was created. *)
  ]

val dyn_of_mkdir_p_result : mkdir_p_result -> Dyn.t
val mkdir_p : ?perms:int -> string -> mkdir_p_result
val mkdir_p_strict : ?perms:int -> string -> [ mkdir_p_result | `Not_a_dir ]

(** [link src dst] creates a hardlink from [src] to [dst]. *)
val link : string -> string -> unit

type follow_symlink_error =
  | Not_a_symlink
  | Max_depth_exceeded
  | Unix_error of Unix_error.Detailed.t

val follow_symlink : string -> (string, follow_symlink_error) result

(** [follow_symlinks path] returns a file path that is equivalent to [path], but
    free of symbolic links. The value [None] is returned if the maximum symbolic
    link depth is reached (i.e., [follow_symlink] returns the value
    [Error Max_depth_exceeded] on some intermediate path). *)
val follow_symlinks : string -> string option

val unlink_exn : ?chmod:bool -> string -> unit
val unlink_no_err : string -> unit

type unlink_status =
  | Success
  | Does_not_exist
  | Is_a_directory
  | Error of exn

(** Unlink and return error, if any. *)
val unlink : string -> unlink_status

val initial_cwd : string

type clear_dir_result =
  | Cleared
  | Directory_does_not_exist

(** [clear_dir t] deletes all the contents of directory [t] without removing [t]
    itself. *)
val clear_dir : ?chmod:bool -> string -> clear_dir_result

(** If the path does not exist, this function is a no-op. *)
val rm_rf : ?chmod:bool -> string -> unit

val is_root : string -> bool

val traverse
  :  dir:string
  -> init:'acc
  -> ?on_file:(dir:string -> Filename.t -> 'acc -> 'acc)
  -> ?on_dir:(dir:string -> Filename.t -> 'acc -> 'acc)
  -> ?on_other:
       [ `Ignore
       | `Raise
       | `Call of dir:string -> Filename.t -> Unix.file_kind -> 'acc -> 'acc
       ]
  -> ?on_symlink:
       [ `Ignore
       | `Resolve
       | `Raise
       | `Call of dir:string -> Filename.t -> 'acc -> 'acc * Unix.file_kind option
       ]
  -> ?enter_dir:(dir:string -> Filename.t -> bool)
  -> ?on_error:
       [ `Ignore | `Raise | `Call of dir:string -> Unix_error.Detailed.t -> 'acc -> 'acc ]
  -> unit
  -> 'acc

val traverse_files
  :  dir:string
  -> init:'acc
  -> f:(dir:string -> Filename.t -> 'acc -> 'acc)
  -> 'acc

(** [is_broken_simlink path] returns [true] iff [path] refers to a symlink
    whose target does not exist.  Returns false if [path] is not a symlink, or
    is a symlink whose target exists. *)
val is_broken_symlink : string -> bool

val is_directory : string -> bool
val exists : string -> bool
