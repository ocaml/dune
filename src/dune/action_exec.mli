open! Stdune

module Context : sig
  type t

  val make :
       targets:Path.Build.Set.t
    -> context:Context.t option
    -> env:Env.t option
    -> t

  val env : t -> Env.t
end

val exec : Action.t -> Context.t -> unit Fiber.t
