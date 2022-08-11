(** Interpret dependencies written in Dune files *)

open Import

(** Alias for all the files in [_build/install] that belong to this package *)
val package_install : context:Build_context.t -> pkg:Package.t -> Alias.t

(** Evaluates unnamed dependency specifications. *)
val unnamed :
     ?sandbox:Sandbox_config.t
  -> expander:Expander.t
  -> Dep_conf.t list
  -> unit Action_builder.t * Sandbox_config.t

(** Evaluates named dependency specifications. Return the action build that
    register dependencies as well as an expander that can be used to expand to
    expand variables from the bindings. *)
val named :
     expander:Expander.t
  -> Dep_conf.t Bindings.t
  -> unit Action_builder.t * Expander.t * Sandbox_config.t
