(** High-level API for compiling OCaml files *)
open! Dune_engine

open! Stdune
open Import

(** Represent a compilation context.

    A compilation context contains all the necessary information to preprocess
    and compile OCaml source files. Exactly one compilation context is
    associated to each library, executable and executables stanza. *)
type t

(** Sets whether [-opaque] is going to be used during compilation. This
    constructs a different dependency graph for native executables. In
    partricular, we can omit dependency on .cmx files. For mli only modules,
    this setting is ignored and is always set when it's available. As there are
    no .cmx files for such modules anyway *)
type opaque =
  | Explicit of bool  (** Set directly by the caller *)
  | Inherit_from_settings
      (** Determined from the version of OCaml and the profile *)

(** Create a compilation context. *)
val create :
     super_context:Super_context.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.t
  -> flags:Ocaml_flags.t
  -> requires_compile:Lib.t list Or_exn.t
  -> requires_link:Lib.t list Or_exn.t Lazy.t
  -> ?preprocessing:Preprocessing.t
  -> opaque:opaque
  -> ?stdlib:Ocaml_stdlib.t
  -> js_of_ocaml:Dune_file.Js_of_ocaml.t option
  -> dynlink:bool
  -> package:Package.t option
  -> ?vimpl:Vimpl.t
  -> ?modes:Dune_file.Mode_conf.Set.Details.t Mode.Dict.t
  -> unit
  -> t

(** Return a compilation context suitable for compiling the alias module. *)
val for_alias_module : t -> t

val super_context : t -> Super_context.t

val expander : t -> Expander.t

val context : t -> Context.t

val scope : t -> Scope.t

val dir : t -> Path.Build.t

val obj_dir : t -> Path.Build.t Obj_dir.t

val modules : t -> Modules.t

val flags : t -> Ocaml_flags.t

val requires_link : t -> Lib.t list Or_exn.t

val requires_compile : t -> Lib.t list Or_exn.t

val includes : t -> Command.Args.dynamic Command.Args.t Cm_kind.Dict.t

val preprocessing : t -> Preprocessing.t

val opaque : t -> bool

val stdlib : t -> Ocaml_stdlib.t option

val js_of_ocaml : t -> Dune_file.Js_of_ocaml.t option

val dynlink : t -> bool

val sandbox : t -> Sandbox_config.t

val package : t -> Package.t option

val vimpl : t -> Vimpl.t option

val modes : t -> Mode.Dict.Set.t

val for_wrapped_compat : t -> t

val for_module_generated_at_link_time :
  t -> requires:Lib.t list Or_exn.t -> module_:Module.t -> t

val for_plugin_executable :
  t -> embed_in_plugin_libraries:(Loc.t * Lib_name.t) list -> t
