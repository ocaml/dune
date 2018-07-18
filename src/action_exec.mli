open Stdune

val exec
  :  targets:Path.Set.t
  -> context:Context.t option
  -> Action.t
  -> unit Fiber.t

(* Return a sandboxed version of an action *)
val sandbox
  :  Action.t
  -> sandboxed:(Path.t -> Path.t)
  -> deps:Path.t list
  -> targets:Path.t list
  -> Action.t
