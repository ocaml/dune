open Import
module Dependency = Dune_action_plugin.Private.Protocol.Dependency

type done_or_more_deps =
  | Done
  (* This code assumes that there can be at most one 'dynamic-run' within single
     action. [DAP.Dependency.t] stores relative paths so name clash would be
     possible if multiple 'dynamic-run' would be executed in different
     subdirectories that contains targets having the same name. *)
  | Need_more_deps of (Dependency.Set.t * Dep.Set.t)

val done_or_more_deps_union : done_or_more_deps -> done_or_more_deps -> done_or_more_deps

module Action_res : sig
  type t =
    { done_or_more_deps : done_or_more_deps
    ; needed_deps : Dep.Set.t
    }

  val union : t -> t -> t
  val done_ : t
  val needed_deps : Dep.Set.t -> t
  val need_more_deps : Dependency.Set.t -> Dep.Set.t -> t
end

val exec
  :  display:Display.t
  -> ectx:Action_intf.Exec.context
  -> eenv:Action_intf.Exec.env
  -> Path.t
  -> string list
  -> Action_res.t Fiber.t
