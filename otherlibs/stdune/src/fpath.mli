(** Functions on paths that are represented as strings *)

type mkdir_result =
  | Already_exists (** The directory already exists. No action was taken. *)
  | Created (** The directory was created. *)
  | Missing_parent_directory
  (** No parent directory, use [mkdir_p] if you want to create it too. *)

val mkdir : ?perms:int -> string -> mkdir_result

type mkdir_p_result =
  | Already_exists (** The directory already exists. No action was taken. *)
  | Created (** The directory was created. *)

val mkdir_p : ?perms:int -> string -> mkdir_p_result

type follow_symlink_error =
  | Not_a_symlink
  | Max_depth_exceeded
  | Unix_error of Dune_filesystem_stubs.Unix_error.Detailed.t

val follow_symlink : string -> (string, follow_symlink_error) result

(** [follow_symlinks path] returns a file path that is equivalent to [path], but
    free of symbolic links. The value [None] is returned if the maximum symbolic
    link depth is reached (i.e., [follow_symlink] returns the value
    [Error Max_depth_exceeded] on some intermediate path). *)
val follow_symlinks : string -> string option

val unlink_exn : string -> unit
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

val clear_dir : string -> clear_dir_result

(** If the path does not exist, this function is a no-op. *)
val rm_rf : string -> unit

val is_root : string -> bool

val traverse
  :  dir:string
  -> init:'acc
  -> on_file:(dir:string -> Filename.t -> 'acc -> 'acc)
  -> on_dir:(dir:string -> Filename.t -> 'acc -> 'acc)
  -> 'acc

val traverse_files
  :  dir:string
  -> init:'acc
  -> f:(dir:string -> Filename.t -> 'acc -> 'acc)
  -> 'acc
