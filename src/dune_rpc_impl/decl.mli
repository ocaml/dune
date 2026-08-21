open Import
open Dune_rpc

(** Internal RPC requests *)

module Status : sig
  module Menu : sig
    type t =
      | Uninitialized
      | Menu of (Method.Name.t * int) list

    val sexp : (t, Conv.values) Conv.t
  end

  type t = { clients : (Id.t * Menu.t) list }

  val sexp : (t, Conv.values) Conv.t
end

module Queue_overflow : sig
  type response =
    | Ok
    | Not_in_watch_mode
    | Build_failed
end

val build : (string list, Build_outcome_with_diagnostics.t) Decl.Request.t
val status : (unit, Status.t) Decl.Request.t
val pkg_enabled : (unit, bool) Decl.Request.t
val simulate_file_watcher_queue_overflow : (unit, Queue_overflow.response) Decl.Request.t
