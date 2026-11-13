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

module Rule_plugin : sig
  type t

  val register
    :  id:Dune_rpc.Id.t
    -> build:Dune_engine.Process.Build.t
    -> exited:unit Fiber.Ivar.t
    -> t

  val initialized : t -> unit Rpc.Server.Session.t option Fiber.t
  val close : t -> unit Fiber.t
end

(** [create ~registry ~root ~build ~where ~action_runner ~required watch_mode]
    is a fresh RPC server.

 Socket creation errors are ignored and the server disabled unless
 [required]. *)
val create
  :  registry:[ `Add | `Skip ]
  -> root:string
  -> build:build
  -> where:Dune_rpc.Where.t
  -> action_runner:Dune_engine.Action_runner.t option
  -> required:bool
  -> Watch_mode_config.t
  -> t

val run : t -> unit Fiber.t
val stop : t -> unit Fiber.t
