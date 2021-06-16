open Dune_rpc_private

type t

module Build_outcome : sig
  type t = Dune_engine.Scheduler.Run.Build_outcome_for_rpc.t =
    | Success
    | Failure
end

module Decl : sig
  open Dune_rpc_private.Decl

  val build : (string list, Build_outcome.t) request

  val shutdown : unit notification

  module Status : sig
    type t = { clients : Id.t list }
  end

  val status : (unit, Status.t) request
end

val create : unit -> t

val config : t -> Run.Config.t

val build_handler : t -> Dune_engine.Build_system.Handler.t

type pending_build_action =
  | Build of Dune_rules.Dep_conf.t list * Build_outcome.t Fiber.Ivar.t

val pending_build_action : t -> pending_build_action Fiber.t
