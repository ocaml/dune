(** Temporary file management *)

(** This module provides a high-level interface for temporary files. It ensures
    that all temporary files created by the application are systematically
    cleaned up on exit. *)

val create : prefix:string -> suffix:string -> Path.t

val destroy : Path.t -> unit
