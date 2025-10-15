open Import

(** The current active RPC server, raising an exception if no RPC server is
    currently running. *)
val active_server_exn : unit -> Dune_rpc.Where.t

(** Raise an RPC response error. *)
val raise_rpc_error : Dune_rpc.Response.Error.t -> 'a

(** Cmdliner term for a generic RPC client. *)
val client_term : Common.Builder.t -> (unit -> 'a Fiber.t) -> 'a

(** Cmdliner argument for a wait flag. *)
val wait_term : bool Cmdliner.Term.t

type ('a, 'b) message_kind =
  | Request : ('a, 'b) Dune_rpc.Decl.request -> ('a, 'b) message_kind
  | Notification : 'a Dune_rpc.Decl.notification -> ('a, unit) message_kind

(** Send a request to the RPC server. If [wait], it will poll forever until a server is listening.
    Should be scheduled by a scheduler that does not come with a RPC server on its own.

    [warn_forwarding] defaults to true, warns the user that since a RPC server is running, some arguments are ignored.
    [lock_held_by] defaults to [Unknown], is only used to allow error messages to print the PID. *)
val fire_message
  :  name:string
  -> wait:bool
  -> ?warn_forwarding:bool
  -> ?lock_held_by:Dune_util.Global_lock.Lock_held_by.t
  -> Common.Builder.t
  -> ('a, 'b) message_kind
  -> 'a
  -> 'b Fiber.t

val wrap_build_outcome_exn
  :  print_on_success:bool
  -> Dune_rpc.Build_outcome_with_diagnostics.t
  -> unit

(** Warn the user that since a RPC server is running, some arguments are ignored. *)
val warn_ignore_arguments : Dune_util.Global_lock.Lock_held_by.t -> unit
