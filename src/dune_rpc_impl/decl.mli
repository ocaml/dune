open Dune_rpc_private

(* Internal RPC requests *)

module Build_outcome : sig
  type t = Dune_engine.Scheduler.Run.Build_outcome.t =
    | Success
    | Failure

  val sexp : (t, Conv.values) Conv.t
end

module Status : sig
  module Menu : sig
    type t =
      | Uninitialized
      | Menu of (string * int) list

    val sexp : (t, Conv.values) Conv.t
  end

  type t = { clients : (Id.t * Menu.t) list }

  val sexp : (t, Conv.values) Conv.t
end

val build : (string list, Build_outcome.t) Decl.Request.t

val status : (unit, Status.t) Decl.Request.t

(** Flush the file watcher queue and returns the number of breakages observed so
    far, as defined by [Scheduler.number_of_breakages]. *)
val flush_file_watcher : (unit, int) Decl.Request.t
