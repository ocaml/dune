open! Stdune

(* For registering the cram_exec function. *)
val cram_run : (env:Env.t -> script:Path.t -> unit Fiber.t) Fdecl.t

(** Type for dependency requested by the dynamic action.

    Must be different from [Dep.t] type because we require it to be
    marshallable. *)
module Dynamic_dep : sig
  type t =
    | File of Path.t
    | Glob of Path.t * Glob.t

  val to_dep : t -> Dep.t

  val compare : t -> t -> Ordering.t

  module Set : sig
    include Set.S with type elt = t

    val to_dep_set : t -> Dep.Set.t
  end
end

module Exec_result : sig
  type t = { dynamic_deps_stages : Dynamic_dep.Set.t List.t }
end

val exec :
     targets:Path.Build.Set.t
  -> context:Build_context.t option
  -> env:Env.t
  -> rule_loc:Loc.t
  -> build_deps:(Dep.Set.t -> unit Fiber.t)
  -> Action.t
  -> Exec_result.t Fiber.t
