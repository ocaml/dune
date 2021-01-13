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
