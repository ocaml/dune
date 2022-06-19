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

  (** Byte compilation with stubs statically linked in, extension [.exe] *)
  val custom : Context.t -> t

  (** [native] if supported, [custom] if not *)
  val native_or_custom : Context.t -> t

  (** Javascript compilation, extension [.bc.js] *)
  val js : t

  val is_native : t -> bool

  val is_js : t -> bool

  val is_byte : t -> bool

  val of_user_config :
    Context.t -> loc:Loc.t -> Dune_file.Executables.Link_mode.t -> t
end

type dep_graphs = { for_exes : Module.t list Action_builder.t list }

(** {1 High-level functions} *)

(** Build and link one or more executables *)

(* [link_many] is like [build_and_link_many], but it allows you to share modules
   between executables without requiring an intermediate library. *)
val link_many :
     ?link_args:Command.Args.without_targets Command.Args.t Action_builder.t
  -> ?o_files:Path.t list
  -> ?embed_in_plugin_libraries:(Loc.t * Lib_name.t) list
  -> ?sandbox:Sandbox_config.t
  -> programs:Program.t list
  -> linkages:Linkage.t list
  -> promote:Rule.Promote.t option
  -> Compilation_context.t
  -> dep_graphs Memo.t

val build_and_link :
     ?link_args:Command.Args.without_targets Command.Args.t Action_builder.t
  -> ?o_files:Path.t list
  -> ?embed_in_plugin_libraries:(Loc.t * Lib_name.t) list
  -> ?sandbox:Sandbox_config.t
  -> program:Program.t
  -> linkages:Linkage.t list
  -> promote:Rule.Promote.t option
  -> Compilation_context.t
  -> dep_graphs Memo.t

val build_and_link_many :
     ?link_args:Command.Args.without_targets Command.Args.t Action_builder.t
  -> ?o_files:Path.t list
  -> ?embed_in_plugin_libraries:(Loc.t * Lib_name.t) list
  -> ?sandbox:Sandbox_config.t
  -> programs:Program.t list
  -> linkages:Linkage.t list
  -> promote:Rule.Promote.t option
  -> Compilation_context.t
  -> dep_graphs Memo.t

val exe_path :
     Compilation_context.t
  -> program:Program.t
  -> linkage:Linkage.t
  -> Path.Build.t
