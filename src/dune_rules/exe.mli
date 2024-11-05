(** Compilation and linking of executables *)

open Import

module Program : sig
  type t =
    { name : string
    ; main_module_name : Module_name.t
    ; loc : Loc.t
    }
end

module Linkage : sig
  type t

  (** Byte compilation, extension [.bc] *)
  val byte : t

  val byte_for_jsoo : t

  (** Native compilation, extension [.exe] *)
  val native : t

  (** like [custom] but allows for a custom extension *)
  val custom_with_ext : ext:string -> Ocaml.Version.t -> t

  (** Byte compilation with stubs statically linked in, extension [.exe] *)
  val custom : Ocaml.Version.t -> t

  (** [native] if supported, [custom] if not *)
  val native_or_custom : Ocaml_toolchain.t -> t

  (** Javascript compilation, extension [.bc.js] *)
  val js : t

  (** Wasm compilation, extension [.bc.wasm.js] *)
  val wasm : t

  val is_native : t -> bool
  val is_jsoo : mode:Js_of_ocaml.Mode.t -> t -> bool
  val is_byte : t -> bool

  val of_user_config
    :  Ocaml_toolchain.t
    -> dynamically_linked_foreign_archives:bool
    -> loc:Loc.t
    -> Executables.Link_mode.t
    -> t
end

type dep_graphs = { for_exes : Module.t list Action_builder.t list }

(** {1 High-level functions} *)

(** Build and link one or more executables *)

(* [link_many] is like [build_and_link_many], but it allows you to share modules
   between executables without requiring an intermediate library. *)
val link_many
  :  ?link_args:Command.Args.without_targets Command.Args.t Action_builder.t
  -> ?o_files:Path.t Mode.Map.Multi.t
  -> ?embed_in_plugin_libraries:(Loc.t * Lib_name.t) list
  -> ?sandbox:Sandbox_config.t
  -> programs:Program.t list
  -> linkages:Linkage.t list
  -> promote:Rule.Promote.t option
  -> Compilation_context.t
  -> dep_graphs Memo.t

val build_and_link
  :  ?link_args:Command.Args.without_targets Command.Args.t Action_builder.t
  -> ?o_files:Path.t Mode.Map.Multi.t
  -> ?embed_in_plugin_libraries:(Loc.t * Lib_name.t) list
  -> ?sandbox:Sandbox_config.t
  -> program:Program.t
  -> linkages:Linkage.t list
  -> promote:Rule.Promote.t option
  -> Compilation_context.t
  -> dep_graphs Memo.t

val build_and_link_many
  :  ?link_args:Command.Args.without_targets Command.Args.t Action_builder.t
  -> ?o_files:Path.t Mode.Map.Multi.t
  -> ?embed_in_plugin_libraries:(Loc.t * Lib_name.t) list
  -> ?sandbox:Sandbox_config.t
  -> programs:Program.t list
  -> linkages:Linkage.t list
  -> promote:Rule.Promote.t option
  -> Compilation_context.t
  -> dep_graphs Memo.t

val exe_path
  :  Compilation_context.t
  -> program:Program.t
  -> linkage:Linkage.t
  -> Path.Build.t
