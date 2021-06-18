open Dune_rpc_private
open Dune_rpc_private.Decl

module Build_outcome : sig
  type t = Dune_engine.Scheduler.Run.Build_outcome_for_rpc.t =
    | Success
    | Failure
end

val build : (string list, Build_outcome.t) request

val shutdown : unit notification

module Status : sig
  type t = { clients : Id.t list }
end

val status : (unit, Status.t) request
