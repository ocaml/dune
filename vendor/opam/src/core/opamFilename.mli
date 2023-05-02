(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Higher level file and directory name manipulation AND file operations,
    wrappers on OpamSystem using the filename type *)

(** Basenames *)
module Base: sig
  include OpamStd.ABSTRACT

  (** Check whether a basename has a given suffix *)
  val check_suffix: t -> string -> bool

  (** Add a file extension *)
  val add_extension: t -> string -> t
end

(** Directory names *)
module Dir: OpamStd.ABSTRACT

(** Return the current working directory *)
val cwd: unit -> Dir.t

(** Remove a directory *)
val rmdir: Dir.t -> unit

(** Cleans the contents of a directory, but keeps the directory in place.
    Noop if directory doesn't exists. *)
val cleandir: Dir.t -> unit

(** Removes an empty directory, as well as any empty leading path components.
    Must be called only on a directory that is known to not have empty parents,
    only internal opam directory (and not tmp dir). *)
val rmdir_cleanup: Dir.t -> unit

(** Create a directory *)
val mkdir: Dir.t -> unit

(** List the sub-directory recursively *)
val rec_dirs: Dir.t -> Dir.t list

val dir_is_empty: Dir.t -> bool

(** List the sub-directory (do not recurse) *)
val dirs: Dir.t -> Dir.t list

(** Evaluate a function in a given directory *)
val in_dir: Dir.t -> (unit -> 'a) -> 'a

(** Turns an assoc list into an array suitable to be provided as environment *)
val env_of_list: (string * string) list -> string array

(** Execute a list of commands in a given directory *)
val exec: Dir.t -> ?env:(string * string) list -> ?name:string ->
  ?metadata:(string * string) list -> ?keep_going:bool -> string list list -> unit

(** Move a directory *)
val move_dir: src:Dir.t -> dst:Dir.t -> unit

(** Copy directory [src] to [dst], that is, recursively copy the contents of
    [src] into [dst], overwriting any existing files. *)
val copy_dir: src:Dir.t -> dst:Dir.t -> unit

(** Link a directory *)
val link_dir: target:Dir.t -> link:Dir.t -> unit

(** Does the directory exist? *)
val exists_dir: Dir.t -> bool

(** Returns the argument as option, if the directory exists *)
val opt_dir: Dir.t -> Dir.t option

(** Return the parent directory *)
val dirname_dir: Dir.t -> Dir.t

(** Return the deeper directory name *)
val basename_dir: Dir.t -> Base.t

(** Turn a full path into a list of directory names *)
val to_list_dir: Dir.t -> Dir.t list

(** Creation from a raw string, without resolving symlinks etc. *)
val raw_dir: string -> Dir.t

(** Execute a function in a temp directory *)
val with_tmp_dir: (Dir.t -> 'a) -> 'a

(** Provide an automatically cleaned up temp directory to a job *)
val with_tmp_dir_job: (Dir.t -> 'a OpamProcess.job) -> 'a OpamProcess.job

(** Raw function to create a temporary directory. No automatic cleanup *)
val mk_tmp_dir: unit -> Dir.t

(** Create a new Dir.t and resolve symlinks *)
val concat_and_resolve: Dir.t -> string -> Dir.t

include OpamStd.ABSTRACT

(** Generic filename *)
type generic_file =
  | D of Dir.t
  | F of t

(** Create a filename from a Dir.t and a basename *)
val create: Dir.t -> Base.t -> t

(** Create a file from a basename and the current working directory
    as dirname *)
val of_basename: Base.t -> t

(** Creation from a raw string, without resolving symlinks, etc. *)
val raw: string -> t

(** Prettify a filename:
    - replace /path/to/opam/foo by <opam>/foo
    - replace /path/to/home/foo by ~/foo *)
val prettify: t -> string

(** Prettify a dirname. *)
val prettify_dir: Dir.t -> string


(** Return the directory name *)
val dirname: t -> Dir.t

(** Return the base name *)
val basename: t -> Base.t

(** Retrieves the contents from the hard disk. *)
val read: t -> string

(** Open a channel from a given file. *)
val open_in: t -> in_channel
val open_in_bin: t -> in_channel
val open_out: t -> out_channel
val open_out_bin: t -> out_channel

(** [with_open_out_bin filename f] opens [f] and passes the out_channel to [f].
    If [f] raises an exception, then [filename] is deleted and then exception is
    propagated. The out_channel does not have to be closed by [f]. *)
val with_open_out_bin: t -> (out_channel -> unit) -> unit

(** As {!with_open_out_bin} except that the file is written atomically. If [f]
    raises an exception, then [filename] will be unaltered. *)
val with_open_out_bin_atomic: t -> (out_channel -> unit) -> unit

(** Removes everything in [filename] if existed. *)
val remove: t -> unit

(** Removes everything in [filename] if existed, then write [contents] instead. *)
val write: t -> string -> unit

(** Returns true if the file exists and is a regular file or a symlink to one *)
val exists: t -> bool

(** Returns the argument as option if it exists and is either a regular file or
    a symlink to one *)
val opt_file: t -> t option

(** Check whether a file has a given suffix *)
val check_suffix: t -> string -> bool

(** Adds a dot and the given file extension *)
val add_extension: t -> string -> t

(** Remove the file extension *)
val chop_extension: t -> t

(** List all the filenames, recursively *)
val rec_files: Dir.t -> t list

(** List all the filename. Do not recurse. *)
val files: Dir.t -> t list

(** Returns alll the files, including dangling symlinks (which means that
    not all entries will satisfy {!exists}). *)
val files_and_links: Dir.t -> t list

(** Apply a function on the contents of a file *)
val with_contents: (string -> 'a) -> t -> 'a

(** Copy a file in a directory. If [root] is set, copy also the
    sub-directories. For instance, [copy_in ~root:"/foo" "/foo/bar/gni"
    "/toto"] creates ["/toto/bar/gni"]. *)
val copy_in: ?root:Dir.t -> t -> Dir.t -> unit

(** Move a file *)
val move: src:t -> dst:t -> unit

(** Read a symlinked file *)
val readlink: t -> t

(** Is a symlink? *)
val is_symlink: t -> bool

val is_symlink_dir: Dir.t -> bool

(** Is an executable? *)
val is_exec: t -> bool

(** Copy a file *)
val copy: src:t -> dst:t -> unit

(** Installs a file to a destination. Optionally set if the destination should
    be set executable *)
val install: ?warning:OpamSystem.install_warning_fn -> ?exec:bool -> src:t -> dst:t -> unit -> unit

(** Symlink a file. If symlink is not possible on the system, use copy instead.
    With [relative], creates a relative link through the closest common ancestor
    directory if possible. Otherwise, the symlink is absolute. *)
val link: ?relative:bool -> target:t -> link:t -> unit

(** Returns true if the given file is an archive (zip or tar) *)
val is_archive: t -> bool

(** Extract an archive in a given directory (it rewrites the root to
    match [Dir.t] dir if needed) *)
val extract: t -> Dir.t -> unit

(** Same as [extract], as an OpamProcess.job *)
val extract_job: t -> Dir.t -> exn option OpamProcess.job

(** Extract an archive in a given directory *)
val extract_in: t -> Dir.t -> unit

val extract_in_job: t -> Dir.t -> exn option OpamProcess.job

val make_tar_gz_job: t -> Dir.t -> exn option OpamProcess.job

(** Extract a generic file *)
val extract_generic_file: generic_file -> Dir.t -> unit

(** Check whether a filename starts by a given Dir.t *)
val starts_with: Dir.t -> t -> bool

(** Check whether a filename ends with a given suffix *)
val ends_with: string -> t -> bool

(** [dir starts_with pfx dir] Check whether [dir] starts with [pfx] *)
val dir_starts_with: Dir.t -> Dir.t -> bool

(** Check whether a dirname ends with a given suffix *)
val dir_ends_with: string -> Dir.t -> bool

(** Remove a prefix from a file name *)
val remove_prefix: Dir.t -> t -> string

val remove_prefix_dir: Dir.t -> Dir.t -> string

(** Remove a suffix from a filename *)
val remove_suffix: Base.t -> t -> string

(** Apply a patch in a directory. If [preprocess] is set to false, there is no
    CRLF translation. Returns [None] on success, the process error otherwise *)
val patch: ?preprocess:bool -> t -> Dir.t -> exn option OpamProcess.job

(** Create an empty file *)
val touch: t -> unit

(** Change file permissions *)
val chmod: t -> int -> unit

(** Returns delay since last file update, based on mtime *)
val written_since: t -> float

(** Returns the closest parent of a directory (including itself) for which the
    predicate holds, if any *)
val find_in_parents: (Dir.t -> bool) ->  Dir.t -> Dir.t option

(** {2 Locking} *)

(** See [OpamSystem.flock]. Prefer the higher level [with_flock] functions when
    possible *)
val flock: [< OpamSystem.lock_flag ] -> ?dontblock:bool -> t -> OpamSystem.lock

(** Calls [f] while holding a lock file. Ensures the lock is properly released
    on [f] exit. Raises [OpamSystem.Locked] if [dontblock] is set and the lock
    can't be acquired. [f] is passed the file_descr of the lock. *)
val with_flock: [< OpamSystem.lock_flag ] -> ?dontblock:bool -> t ->
  (Unix.file_descr -> 'a) -> 'a

(** Calls [f] with the file lock upgraded to at least [flag], then restores the
    previous lock level. Upgrade to [`Lock_write] should never be used in
    blocking mode as it would deadlock. Raises [OpamSystem.Locked] (but keeps
    the lock as is) if [dontblock] is set and the lock can't be upgraded. *)
val with_flock_upgrade:
  [< OpamSystem.actual_lock_flag ] -> ?dontblock:bool -> OpamSystem.lock -> (Unix.file_descr -> 'a) -> 'a

(** Runs first function with a write lock on the given file, then releases it to
    a read lock and runs the second function. *)
val with_flock_write_then_read:
  ?dontblock:bool -> t -> (Unix.file_descr -> 'a) -> ('a -> 'b) -> 'b

module Op: sig

  (** Create a new directory *)
  val (/): Dir.t -> string -> Dir.t

  (** Create a new filename *)
  val (//): Dir.t -> string -> t

end

(** Simple structure to handle file attributes *)
module Attribute: sig

  include OpamStd.ABSTRACT

  val to_string_list: t -> string list

  val of_string_list: string list -> t

  (** Get remote filename *)
  val base: t -> Base.t

  (** MD5 digest of the remote file *)
  val md5: t -> OpamHash.t

  (** File permission *)
  val perm: t -> int option

  (** Constructor*)
  val create: Base.t -> OpamHash.t -> int option -> t

end

(* Subpathes handling *)
module SubPath: sig

  include OpamStd.ABSTRACT

  (* Directory concatenation *)
  val (/): Dir.t -> t -> Dir.t

  (* Directory concatenation with an optional argument *)
  val (/?): Dir.t -> t option -> Dir.t

  (* Subpath string with no file separator conversion *)
  val normalised_string: t -> string

end

(** Convert a filename to an attribute, relatively to a root *)
val to_attribute: Dir.t -> t -> Attribute.t
