open! Stdune

val exec_env : context:Context.t option -> env:Env.t option -> Env.t

val exec :
     targets:Path.Build.Set.t
  -> context:Context.t option
  -> env:Env.t option
  -> Action.t
  -> unit Fiber.t
