open Import

module Sandbox_actions_backend : sig
  type t =
    | Auto
    | Bwrap
    | Landlock

  val all : (string * t) list
  val equal : t -> t -> bool
  val default : t
end

val create
  :  where:Dune_rpc.Where.t
  -> config:Dune_config.t
  -> sandbox_actions:bool
  -> sandbox_actions_backend:Sandbox_actions_backend.t
  -> Dune_engine.Action_runner.t

val start_worker : name:string -> where:string -> trace_fd:string option -> unit Fiber.t
