open Import

(** The current active RPC server, raising an exception if no RPC server is
    currently running. *)
val active_server_exn : unit -> Dune_rpc.Where.t

(** Raise an RPC response error. *)
val raise_rpc_error : Dune_rpc.Response.Error.t -> 'a

(** Make a request and raise an exception if the preparation for the request
    fails in any way. Returns an [Error] if the response errors. *)
val request_exn
  :  Dune_rpc_client.Client.t
  -> ('a, 'b) Dune_rpc.Decl.Request.witness
  -> 'a
  -> ('b, Dune_rpc.Response.Error.t) result Fiber.t

(** Cmdliner term for a generic RPC client. *)
val client_term : Common.Builder.t -> (unit -> 'a Fiber.t) -> 'a

(** Cmdliner argument for a wait flag. *)
val wait_term : bool Cmdliner.Term.t

(** Send a request to the RPC server. If [wait], it will poll forever until a server is listening.
    Should be scheduled by a scheduler that does not come with a RPC server on its own. *)
val fire_request
  :  name:string
  -> wait:bool
  -> ('a, 'b) Dune_rpc.Decl.request
  -> 'a
  -> ('b, Dune_rpc.Response.Error.t) result Fiber.t

val wrap_build_outcome_exn
  :  print_on_success:bool
  -> ('a
      -> (Dune_rpc.Build_outcome_with_diagnostics.t, Dune_rpc.Response.Error.t) result
           Fiber.t)
  -> 'a
  -> unit
  -> unit Fiber.t

(**  Schedule a fiber to run via RPC, wrapping any errors. *)
val run_via_rpc
  :  builder:Common.Builder.t
  -> common:Common.t
  -> config:Dune_config_file.Dune_config.t
  -> Dune_util.Global_lock.Lock_held_by.t
  -> ('a
      -> (Dune_rpc.Build_outcome_with_diagnostics.t, Dune_rpc.Response.Error.t) result
           Fiber.t)
  -> 'a
  -> unit
