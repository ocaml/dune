(** Preprocessing of OCaml source files *)

open! Import

(** Preprocessing object *)
type t

val make
  :  Super_context.t
  -> dir:Path.t
  -> dep_kind:Build.lib_dep_kind
  -> lint:Jbuild.Preprocess_map.t
  -> preprocess:Jbuild.Preprocess_map.t
  -> preprocessor_deps:(unit, Path.t list) Build.t
  -> lib_name:string option
  -> scope:Scope.t
  -> t

(** Setup the preprocessing rules for the following modules and
    returns the translated modules *)
val pp_modules
  :  t
  -> ?lint:bool
  -> Module.t Module.Name.Map.t
  -> Module.t Module.Name.Map.t

(** Preprocess a single module, using the configuration for the given
    module name. *)
val pp_module_as
  :  t
  -> ?lint:bool
  -> Module.Name.t
  -> Module.t
  -> Module.t

(** Get a path to a cached ppx driver *)
val get_ppx_driver
  : Super_context.t
  -> scope:Scope.t
  -> (Loc.t * Jbuild.Pp.t) list
  -> Path.t

(** [cookie_library_name lib_name] is ["--cookie"; lib_name] if [lib_name] is not
    [None] *)
val cookie_library_name : string option -> string list

val gen_rules : Super_context.t -> string list -> unit
