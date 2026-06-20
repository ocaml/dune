open Import

(** Action runners are instances capable of executing build processes outside
    of the main dune process.

    They are intended for isolating unsafe actions. For now, eligible actions
    are user-defined actions and preprocessor invocations. They are not meant
    to run performance-sensitive or explicitly whitelisted toolchain commands,
    such as compiler invocations or ocamldep. *)

type t

module Rpc_server : sig
    type action_runner

    (** The server-side component responsible for orchestrating action runners. *)
    type t

    val create : [ `Disabled | `Enabled of action_runner ] -> t

    (** [implement_handler t handler] wires the action runner requests into an
      existing RPC handler. *)
    val implement_handler : t -> 'a Root.Rpc.Server.Handler.t -> unit

    (** [run t] is to be run by the RPC server. *)
    val run : t -> unit Fiber.t

    (** [stop t] is to be run by the RPC server. *)
    val stop : t -> unit Fiber.t
  end
  with type action_runner := t

val create : Action_runner_name.t -> Pid.t -> t
val ensure_ready : t -> unit Fiber.t

(** [exec_process t ~run_id ~cancellation process] dispatches [process] to [t]
    as part of [run_id]. If [cancellation] fires, the corresponding build is
    cancelled in the worker. *)
val exec_process
  :  t
  -> run_id:Run_id.t
  -> cancellation:Fiber.Cancel.t
  -> Process_runner.request
  -> Process_runner.response Fiber.t
