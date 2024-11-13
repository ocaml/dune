(** High-level API for compiling OCaml files *)

open Import

(** Represent a compilation context.

    A compilation context contains all the necessary information to preprocess
    and compile OCaml source files. Exactly one compilation context is
    associated to each library, executable and executables stanza. *)
type t

(** Sets whether [-opaque] is going to be used during compilation. This
    constructs a different dependency graph for native executables. In
    particular, we can omit dependency on .cmx files. For mli only modules, this
    setting is ignored and is always set when it's available. As there are no
    .cmx files for such modules anyway *)
type opaque =
  | Explicit of bool (** Set directly by the caller *)
  | Inherit_from_settings (** Determined from the version of OCaml and the profile *)

(** Create a compilation context. *)
val create
  :  super_context:Super_context.t
  -> scope:Scope.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> flags:Ocaml_flags.t
  -> requires_compile:Lib.t list Resolve.Memo.t
  -> requires_link:Lib.t list Resolve.t Memo.Lazy.t
  -> ?preprocessing:Pp_spec.t
  -> opaque:opaque
  -> ?stdlib:Ocaml_stdlib.t
  -> js_of_ocaml:Js_of_ocaml.In_context.t option Js_of_ocaml.Mode.Pair.t
  -> package:Package.t option
  -> melange_package_name:Lib_name.t option
  -> ?vimpl:Vimpl.t
  -> ?modes:Mode_conf.Set.Details.t Lib_mode.Map.t
  -> ?bin_annot:bool
  -> ?loc:Loc.t
  -> unit
  -> t Memo.t

(** Return a compilation context suitable for compiling the alias module. *)
val for_alias_module : t -> Module.t -> t

val super_context : t -> Super_context.t
val context : t -> Context.t
val scope : t -> Scope.t

(** [dir] should only be used for adding rules. It should be used to access
    dependencies or targets. *)
val dir : t -> Path.Build.t

val obj_dir : t -> Path.Build.t Obj_dir.t
val modules : t -> Modules.With_vlib.t
val flags : t -> Ocaml_flags.t
val requires_link : t -> Lib.t list Resolve.Memo.t
val requires_hidden : t -> Lib.t list Resolve.Memo.t
val requires_compile : t -> Lib.t list Resolve.Memo.t
val includes : t -> Command.Args.without_targets Command.Args.t Lib_mode.Cm_kind.Map.t
val preprocessing : t -> Pp_spec.t
val opaque : t -> bool
val stdlib : t -> Ocaml_stdlib.t option
val js_of_ocaml : t -> Js_of_ocaml.In_context.t option Js_of_ocaml.Mode.Pair.t
val sandbox : t -> Sandbox_config.t
val set_sandbox : t -> Sandbox_config.t -> t
val package : t -> Package.t option
val vimpl : t -> Vimpl.t option
val melange_package_name : t -> Lib_name.t option
val modes : t -> Lib_mode.Map.Set.t
val for_wrapped_compat : t -> t
val for_root_module : t -> Module.t -> t
val ocaml : t -> Ocaml_toolchain.t

val for_module_generated_at_link_time
  :  t
  -> requires:Lib.t list Resolve.Memo.t
  -> module_:Module.t
  -> t

val for_plugin_executable : t -> embed_in_plugin_libraries:(Loc.t * Lib_name.t) list -> t
val bin_annot : t -> bool
val without_bin_annot : t -> t
val root_module_entries : t -> Module_name.t list Action_builder.t

(** The dependency graph for the modules of the library. *)
val dep_graphs : t -> Dep_graph.t Ml_kind.Dict.t

val ocamldep_modules_data : t -> Ocamldep.Modules_data.t
val loc : t -> Loc.t option
val set_obj_dir : t -> Path.Build.t Obj_dir.t -> t
val set_modes : t -> modes:Lib_mode.Map.Set.t -> t
