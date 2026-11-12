(** Context-dependent package selection for shared install layouts. *)

open Import

(** Resolve workspace and opaque providers, then materialize their outputs
    through [Install_layout]. *)
val materialize
  :  Context_name.t
  -> Package.Name.Set.t
  -> Install_layout.t Action_builder.t

(** Materialize an owner's recipe dependency closure in dependency order and
    return its direct dependencies. Workspace and locked providers contribute
    through the same materialiser variables. *)
val materialize_dependencies
  :  Context_name.t
  -> Package.Name.t
  -> (Package.Name.Set.t * Install_layout.t) Action_builder.t
