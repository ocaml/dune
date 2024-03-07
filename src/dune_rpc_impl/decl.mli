open Import
open Dune_rpc

(** Internal RPC requests *)

module Build_outcome : sig
  type t = Scheduler.Run.Build_outcome.t =
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
