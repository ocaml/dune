open Import

(** Action runners are instances capable of executing build processes outside
    of the main dune process. *)

type t
type worker = t

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
val set_start : t -> (runner:worker -> generation:int -> unit Fiber.t) -> unit
val disconnect : ?generation:int -> t -> unit Fiber.t

(** [exec_process worker process] dispatches [process] to [worker]. *)
val exec_process
  :  t
  -> run_id:Dune_scheduler.Run_id.t
  -> Process.Runner.request
  -> Process.Runner.response Fiber.t

(** [cancel_build] cancels the current run on [worker] and only returns once
    that run has drained. *)
val cancel_build : t -> run_id:Dune_scheduler.Run_id.t -> unit Fiber.t

module Worker : sig
  (** [start ~name ~generation ~where] starts a runner named [name] connected to
      the main dune RPC server listening at [where]. *)
  val start
    :  name:Action_runner_name.t
    -> generation:int
    -> where:Dune_rpc.Where.t
    -> unit Fiber.t
end
