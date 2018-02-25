(** Compilation and linking of executables *)

open Import

module Program : sig
  type t =
    { name             : string
    ; main_module_name : string
    }
end

module Linkage : sig
  type t

  (** Byte compilation, exetension [.bc] *)
  val byte : t

  (** Native compilation, extension [.exe] *)
  val native : t

  (** Byte compilation, link with [-custom], extension [.exe] *)
  val custom : t

  (** [native] if supported, [custom] if not *)
  val native_or_custom : Context.t -> t

  val make
    :  mode:Mode.t
    -> ext:string
    -> ?flags:string list
    -> unit
    -> t
end

(** {1 High-level functions} *)

(** Build and link one or more executables *)

val build_and_link
  :  dir:Path.t
  -> obj_dir:Path.t
  -> program:Program.t
  -> modules:Module.t String_map.t
  -> scope:Scope.t
  -> linkages:Linkage.t list
  -> ?requires:(unit, Lib.L.t) Build.t
  -> ?already_used:String_set.t
  -> ?flags:Ocaml_flags.t
  -> ?link_flags:(unit, string list) Build.t
  -> ?js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> Super_context.t
  -> unit

val build_and_link_many
  :  dir:Path.t
  -> obj_dir:Path.t
  -> programs:Program.t list
  -> modules:Module.t String_map.t
  -> scope:Scope.t
  -> linkages:Linkage.t list
  -> ?requires:(unit, Lib.L.t) Build.t
  -> ?already_used:String_set.t
  -> ?flags:Ocaml_flags.t
  -> ?link_flags:(unit, string list) Build.t
  -> ?js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> Super_context.t
  -> unit

(** {1 Low-level functions} *)

(** Link a single executable *)
val link_exe
  :  dir:Path.t
  -> obj_dir:Path.t
  -> scope:Scope.t
  -> requires:(unit, Lib.t list) Build.t
  -> name:string
  -> linkage:Linkage.t
  -> top_sorted_modules:(unit, Module.t list) Build.t
  -> ?flags:Ocaml_flags.t
  -> ?link_flags:(unit, string list) Build.t
  -> ?js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> Super_context.t
  -> unit
