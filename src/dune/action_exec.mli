open! Stdune

module Exec_result : sig
  type t = { dynamic_deps_stages : Dep.Set.t List.t }
end

val exec :
     targets:Path.Build.Set.t
  -> context:Context.t option
  -> env:Env.t
  -> rule_loc:Loc.t
  -> build_deps:(Dep.Set.t -> unit Fiber.t)
  -> Action.t
  -> Exec_result.t Fiber.t
