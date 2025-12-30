open Stdune

(** An RPC handler which is abstract over the handling of the "Build" request
    type. The type argument allows instances to choose different
    representations of build targets. *)
type 'build_arg t

val create
  :  lock_timeout:Time.Span.t option
  -> registry:[ `Add | `Skip ]
  -> root:string
  -> handle:(unit Dune_rpc_server.Handler.t -> unit)
       (** register additional requests or notifications *)
  -> parse_build_arg:(string -> Dune_lang.Dep_conf.t)
  -> Dune_lang.Dep_conf.t t

type 'build_arg pending_action_kind =
  | Build of 'build_arg list
  | Runtest of string list

(** This type allows the build request handler to be defined externally to the
    RPC server. The [outcome] ivar is expected to be filled with the outcome of
    the build by the build request handler when the build completes
    (successfully or not) and triggers the RPC server to reply to the client
    with the outcome of their request. *)
type 'build_arg pending_action =
  { kind : 'build_arg pending_action_kind
  ; outcome : Dune_engine.Scheduler.Run.Build_outcome.t Fiber.Ivar.t
  }

val pending_action : 'build_arg t -> 'build_arg pending_action Fiber.t

(** Stop accepting new rpc connections. Fiber returns when all existing
    connections terminate *)
val stop : _ t -> unit Fiber.t

val ready : _ t -> unit Fiber.t
val run : _ t -> unit Fiber.t
