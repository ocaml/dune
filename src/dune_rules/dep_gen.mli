open Import

module Modules_data : sig
  (** Various information needed about a set of modules.

      This is a subset of [Compilation_context]. We don't use
      [Compilation_context] directory as this would create a circular
      dependency. *)
  type t =
    { dir : Path.Build.t
    ; obj_dir : Path.Build.t Obj_dir.t
    ; sctx : Super_context.t
    ; vimpl : Vimpl.t option
    ; modules : Modules.t
    ; stdlib : Ocaml_stdlib.t option
    ; sandbox : Sandbox_config.t
    ; project : Dune_project.t
    }
end

val parse_module_names :
     dir:Path.Build.t
  -> unit:Module.t
  -> modules:Modules.t
  -> string list
  -> Module.t list

val parse_deps_exn : file:Path.t -> string list -> string list

val read_deps_of :
     obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t

val read_immediate_deps_of_source :
     obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.t
  -> file:Path.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t

val transitive_of_immediate_rule :
     Modules_data.t
  -> ml_kind:Ml_kind.t
  -> file:Path.t
  -> Module.t
  -> unit Memo.t

module type S = sig
  val deps_of :
       Modules_data.t
    -> ml_kind:Ml_kind.t
    -> Module.t
    -> Module.t list Action_builder.t Memo.t

  (** [read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit] returns the
      immediate dependencies found in the modules of [modules] for the file with
      kind [ml_kind] of the module [unit]. If there is no such file with kind
      [ml_kind], then an empty list of dependencies is returned. *)
  val read_immediate_deps_of :
       obj_dir:Path.Build.t Obj_dir.t
    -> modules:Modules.t
    -> ml_kind:Ml_kind.t
    -> Module.t
    -> Module.t list Action_builder.t
end
