(** A specification for preprocessing modules. To setup rules based on this
    specification, use [Pp_spec_rules] *)

open Import

type t

val dummy : t
val make : (Module.t -> lint:bool -> Module.t Memo.t) Module_name.Per_item.t -> t

(** Setup the preprocessing rules for the following modules and returns the
    translated modules *)
val pp_module : t -> ?lint:bool -> Module.t -> Module.t Memo.t

(** Preprocess a single module, using the configuration for the given module
    name. *)
val pp_module_as : t -> ?lint:bool -> Module_name.t -> Module.t -> Module.t Memo.t

val pped_modules_map
  :  Preprocess.Without_instrumentation.t Preprocess.t Module_name.Per_item.t
  -> Ocaml.Version.t
  -> (Module.t -> Module.t) Staged.t
