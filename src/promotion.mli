open Stdune

module File : sig
  type t =
    { src : Path.t
    ; dst : Path.t
    }

  (** Register a file to promote *)
  val register : t -> unit
end

(** Promote all registered files if [!Clflags.auto_promote]. Otherwise dump the list of
    registered files to [_build/.to-promote]. *)
val finalize : unit -> unit

val promote_files_registered_in_last_run : unit -> unit
