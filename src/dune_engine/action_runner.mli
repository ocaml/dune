open Import

(** Action runners are instances capable of executing dune actions outside of
    the build engine's process. *)

module Name : sig
  type t

  val of_string : string -> t
  val of_string_opt : string -> t option
  val parse_string_exn : Loc.t * string -> t
  val to_string : t -> string
  val repr : t Repr.t
end

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

val create : Rpc_server.t -> name:Name.t -> t
val name : t -> Name.t
val await_ready : t -> unit Fiber.t
val disconnect : t -> unit Fiber.t

(** [exec_action worker action] dispatches [action] to [worker]. *)
val exec_action
  :  t
  -> run_id:Dune_scheduler.Run_id.t
  -> Action_exec.input
  -> Action_exec.Exec_result.t Fiber.t

(** [cancel_build] cancels all actions being executed by [worker]. *)
val cancel_build : t -> run_id:Dune_scheduler.Run_id.t -> unit Fiber.t

module Worker : sig
  (** [start ~name ~where] starts a runner named [name] connected to the main
      dune RPC server listening at [where]. *)
  val start : name:Name.t -> where:Dune_rpc.Where.t -> unit Fiber.t
end
