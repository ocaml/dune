(** Action runners are instances capabale of executing dune actions outside of
    the build engine's process. *)

type t

module Rpc_server : sig
  (** The component of the RPC server required to orchestrate the runners. It's
      responsible for handing off sessions to action runners once they connect. *)
  type runner := t

  type t

  val create : unit -> t

  (** [implement_handler handler] adds the functions needed to manage connected
      workers to the rpc server. This only needs to be called by the RPC server *)
  val implement_handler : t -> 'a Dune_rpc_server.Handler.t -> unit

  (** [run t] is to be run by the rpc server *)
  val run : t -> unit Fiber.t

  (** [stop t] is to be run by the rpc server *)
  val stop : t -> unit Fiber.t

  val all_runners : t -> runner list
end

val create : Rpc_server.t -> name:string -> t
val name : t -> string

(* CR-soon dkalinichenko: return [Exn_with_backtrace.t list] in the error case
   after rgrinberg patches exception marshalling upstream. *)

(** [exec_action worker action] dispatches [action] to [worker] *)
val exec_action : t -> Action_exec.input -> Action_exec.Exec_result.t Fiber.t

(** [cancel_build] cancels all actions being executed by [worker] *)
val cancel_build : t -> unit Fiber.t

module Worker : sig
  (** A worker is a runner of action *)

  (** [start ~name ~where] start a runner named [name] connected to server
      [where]. The server is allowed to dispatch actions to this worker once the
      worker initializes itself.

      This function returns when the connection to the server terminates. *)
  val start : name:string -> where:Dune_rpc_private.Where.t -> unit Fiber.t
end
