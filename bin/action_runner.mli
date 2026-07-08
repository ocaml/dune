open Import

module Backend : sig
  type t =
    | Bwrap
    | Landlock

  val equal : t -> t -> bool
  val to_string : t -> string
  val conv : t Arg.conv
end

val create
  :  where:Dune_rpc.Where.t
  -> config:Dune_config.t
  -> sandbox_actions:bool
  -> sandbox_actions_backends:Backend.t list
  -> Dune_engine.Action_runner.t

val start_worker : name:string -> where:string -> trace_fd:string option -> unit Fiber.t
