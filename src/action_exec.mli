open Stdune

module Promotion : sig
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
end

val exec
  :  targets:Path.Set.t
  -> context:Context.t option
  -> Action.t
  -> unit Fiber.t

(* Return a sandboxed version of an action *)
val sandbox
  :  Action.t
  -> sandboxed:(Path.t -> Path.t)
  -> deps:Path.t list
  -> targets:Path.t list
  -> Action.t
