(** The current active RPC server. *)
val active_server : unit -> Dune_rpc_private.Where.t

(** Raise an RPC response error. *)
val raise_rpc_error : Dune_rpc_private.Response.Error.t -> 'a

(** Make a request and raise an exception if the preparation for the request
    fails in any way. Returns an [Error] if the response errors. *)
val request_exn
  :  Dune_rpc_client.Client.t
  -> ('a, 'b) Dune_rpc_private.Decl.Request.witness
  -> 'a
  -> ('b, Dune_rpc_private.Response.Error.t) result Fiber.t

(** Cmdliner term for a generic RPC client. *)
val client_term : Common.Builder.t -> (unit -> 'a Fiber.t) -> 'a

(** Cmdliner argument for a wait flag. *)
val wait_term : bool Cmdliner.Term.t
