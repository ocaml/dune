open Import

(** An RPC handler. *)
type t

type build_request =
  | Build of Dune_lang.Dep_conf.t list
  | Runtest of string list

type build =
  | Disabled
  | Enabled of
      { build_loop : Dune_engine.Build_loop.t
      ; build_action : build_request -> unit Dune_engine.Action_builder.t
      }

val create
  :  registry:[ `Add | `Skip ]
  -> root:string
  -> build:build
  -> Watch_mode_config.t
  -> t

(** Stop accepting new rpc connections. Fiber returns when all existing
    connections terminate *)
val stop : t -> unit Fiber.t

val ready : t -> unit Fiber.t
val run : t -> unit Fiber.t
val listening_address : t -> Dune_rpc.Where.t
val action_runner : t -> Dune_engine.Action_runner.Rpc_server.t
