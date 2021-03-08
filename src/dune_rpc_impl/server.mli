open Dune_rpc_private

type t

module Status : sig
  type t =
    | Accepted
    | Rejected
end

module Decl : sig
  open Dune_rpc_private.Decl

  val build : (string list, Status.t) request

  val ping : (unit, unit) request

  val shutdown : unit notification

  module Status : sig
    type t = { clients : Id.t list }
  end

  val status : (unit, Status.t) request
end

val create : unit -> t

val build_mutex : t -> Fiber.Mutex.t

val config : t -> Dune_engine.Scheduler.Config.Rpc.t

type pending_build_action =
  | Shutdown
  | Build of Dune_rules.Dep_conf.t list * Status.t Fiber.Ivar.t

val pending_build_action : t -> pending_build_action option
