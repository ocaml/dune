open Import

(** Action runners are instances capable of executing build processes outside
    of the main dune process. *)

type t

module Rpc_server : sig
  (** The server-side component responsible for orchestrating action runners. *)
  type t

  val create : unit -> t

  (** [implement_handler t handler] wires the action runner requests into an
      existing RPC handler. *)
  val implement_handler : t -> 'a Root.Rpc.Server.Handler.t -> unit

  (** [run t] is to be run by the RPC server. *)
  val run : t -> unit Fiber.t

  (** [stop t] is to be run by the RPC server. *)
  val stop : t -> unit Fiber.t
end

val create : Rpc_server.t -> name:Action_runner_name.t -> t
val name : t -> Action_runner_name.t
val set_start : t -> (runner:t -> generation:int -> unit Fiber.t) -> unit
val disconnect : ?generation:int -> t -> unit Fiber.t

(** [exec_process t ~run_id ~cancellation process] dispatches [process] to [t]
    as part of [run_id]. If [cancellation] fires, the corresponding build is
    cancelled in the worker. *)
val exec_process
  :  t
  -> run_id:Run_id.t
  -> cancellation:Fiber.Cancel.t
  -> Process_runner.request
  -> Process_runner.response Fiber.t

(** [cancel_build] cancels the current run on [t] and only returns once that
    run has drained. *)
val cancel_build : t -> run_id:Run_id.t -> unit Fiber.t
