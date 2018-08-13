(** Compilation and linking of executables *)

module Program : sig
  type t =
    { name             : string
    ; main_module_name : Module.Name.t
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

  val of_user_config : Context.t -> Dune_file.Executables.Link_mode.t -> t
end

(** {1 High-level functions} *)

(** Build and link one or more executables *)

val build_and_link
  :  program:Program.t
  -> linkages:Linkage.t list
  -> ?link_flags:(unit, string list) Build.t
  -> ?js_of_ocaml:Dune_file.Js_of_ocaml.t
  -> Compilation_context.t
  -> unit

val build_and_link_many
  :  programs:Program.t list
  -> linkages:Linkage.t list
  -> ?link_flags:(unit, string list) Build.t
  -> ?js_of_ocaml:Dune_file.Js_of_ocaml.t
  -> Compilation_context.t
  -> unit

(** {1 Low-level functions} *)

(** Link a single executable *)
val link_exe
  :  name:string
  -> linkage:Linkage.t
  -> top_sorted_modules:(unit, Module.t list) Build.t
  -> ?link_flags:(unit, string list) Build.t
  -> ?js_of_ocaml:Dune_file.Js_of_ocaml.t
  -> Compilation_context.t
  -> unit
