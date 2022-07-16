open Import

(* cwong: Maybe this shouldn't go here? *)
val raise_rpc_error : Dune_rpc_private.Response.Error.t -> unit

val active_server : unit -> Dune_rpc.Where.t

val client_term : Common.t -> (unit -> 'a Fiber.t) -> 'a

val group : unit Term.Group.t
