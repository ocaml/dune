(** Temporary file management *)

(** This module provides a high-level interface for temporary files. It ensures
    that all temporary files created by the application are systematically
    cleaned up on exit. *)

type what =
  | Dir
  | File

(** Create a temporary file or directory inside an existing directory. *)
val temp_in_dir
  :  ?perms:int
  -> what
  -> dir:Path.t
  -> prefix:string
  -> suffix:string
  -> Path.t

val create : ?perms:int -> what -> prefix:string -> suffix:string -> Path.t
val destroy : what -> Path.t -> unit

(** Delete the contents of a temporary directory without deleting the directory
    itself. *)
val clear_dir : Path.t -> unit

(** [temp_file ~dir ~prefix ~suffix] creates a temporary file in [dir]. The base
    name of the file is [prefix_num_suffix], where [num] is a random integer
    number. Note that the file must be created to reserve the name in [dir] and
    prevent other processes from taking it concurrently. If you need a temporary
    file that does not exist on disk, you can create a temporary directory and
    safely use any file name there. *)
val temp_file : dir:Path.t -> prefix:string -> suffix:string -> Path.t

(** Like [temp_file], but passes the temporary file to the callback [f], and
    makes sure the temporary file is deleted when [f] completes. If [f] raises
    an exception, the exception is re-raised (and the file is still deleted). *)
val with_temp_file
  :  dir:Path.t
  -> prefix:string
  -> suffix:string
  -> f:(Path.t Or_exn.t -> 'a)
  -> 'a

(** Like [with_temp_file], but creates a temporary directory. *)
val with_temp_dir
  :  parent_dir:Path.t
  -> prefix:string
  -> suffix:string
  -> f:(Path.t Or_exn.t -> 'a)
  -> 'a

(** Versions of [with_temp_file] and [with_temp_dir] that are suitable for use
    with concurrency monads. *)
module Monad (M : sig
    type 'a t

    (** Like [Exn.protect] but lifted to [M]. *)
    val protect : f:(unit -> 'a t) -> finally:(unit -> unit) -> 'a t
  end) : sig
  val with_temp_file
    :  dir:Path.t
    -> prefix:string
    -> suffix:string
    -> f:(Path.t Or_exn.t -> 'a M.t)
    -> 'a M.t

  val with_temp_dir
    :  parent_dir:Path.t
    -> prefix:string
    -> suffix:string
    -> f:(Path.t Or_exn.t -> 'a M.t)
    -> 'a M.t
end
