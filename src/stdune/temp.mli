(** Temporary file management *)

(** This module provides a high-level interface for temporary files. It ensures
    that all temporary files created by the application are systematically
    cleaned up on exit. *)

val create : prefix:string -> suffix:string -> Path.t

val destroy : Path.t -> unit

module Dir : sig
  (** Temporary directory API*)
  type t

  val create : for_script:string -> t

  val open_file : t -> suffix:string -> Path.t * out_channel

  val file : t -> suffix:string -> Path.t
end
