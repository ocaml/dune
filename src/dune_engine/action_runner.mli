(** Action runners are instances capabale of executing dune actions outside of
    the build engine's process. *)

module Rpc_server : sig
  (** The component of the RPC server required to orchestrate the runners. It's
      responsible for handing off sessions to action runners once they connect. *)

  type t

  val create : unit -> t

  (** [implement_handler handler] adds the functions needed to manage connected
      workers to the rpc server. This only needs to be called by the RPC server *)
  val implement_handler : t -> 'a Dune_rpc_server.Handler.t -> unit

  (** [run t] is to be run by the rpc server *)
  val run : t -> unit Fiber.t

  (** [stop t] is to be run by the rpc server *)
  val stop : t -> unit Fiber.t
end

type t

val create : Rpc_server.t -> name:string -> t

val name : t -> string

(** [exec_action worker action] dispatches [action] to [worker] *)
val exec_action : t -> Action_exec.input -> Action_exec.Exec_result.t Fiber.t

module Worker : sig
  (** A worker is a runner of action *)

  (** [start ~name ~where] start a runner named [name] connected to server
      [where]. The server is allowed to dispatch actions to this worker once the
      worker initializes itself.

      This function returns when the connection to the server terminates. *)
  val start : name:string -> where:Dune_rpc_private.Where.t -> unit Fiber.t
end
