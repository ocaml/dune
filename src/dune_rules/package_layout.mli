(** Context-dependent package selection for shared install layouts. *)

open Import

(** Resolve workspace and opaque providers, then materialize their outputs
    through [Install_layout]. *)
val materialize
  :  Context_name.t
  -> Package.Name.Set.t
  -> Install_layout.t Action_builder.t
