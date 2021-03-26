(** Temporary file management *)

(** This module provides a high-level interface for temporary files. It ensures
    that all temporary files created by the application are systematically
    cleaned up on exit. *)

type what =
  | Dir
  | File

(** Create a temporary file or directory inside an existing directory*)
val temp_in_dir :
  ?perms:int -> what -> dir:Path.t -> prefix:string -> suffix:string -> Path.t

val create : ?perms:int -> what -> prefix:string -> suffix:string -> Path.t

val destroy : what -> Path.t -> unit

(** Delete the contents of a temporary directory without deleting the directory
    itself. *)
val clear_dir : Path.t -> unit

(** [temp_path ~dir ~prefix ~suffix] generate a temporary path in [dir]. The
    base name of the temporary file is formed by concatenating [prefix], then a
    suitably chosen integer number, then [suffix]. *)
val temp_path : dir:Path.t -> prefix:string -> suffix:string -> Path.t

(** Like [temp_path], but passes the temporary file to the callback [f], and
    makes sure the temporary file is deleted when [f] completes. If [f] raises
    an exception, the exception is reraised (and the file is still deleted). *)
val with_temp_path :
     dir:Path.t
  -> prefix:string
  -> suffix:string
  -> f:((Path.t, exn) result -> 'a)
  -> 'a
