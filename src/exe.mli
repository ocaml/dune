(** Compilation and linking of executables *)
open Stdune

module Program : sig
  type t =
    { name             : string
    ; main_module_name : Module.Name.t
    ; loc              : Loc.t
    }
end

module Linkage : sig
  type 'mode t

  val map : 'm1 t -> f:('m1 -> 'm2) -> 'm2 t

  (** Byte compilation, exetension [.bc] *)
  val byte : Mode.Js.t t

  (** Native compilation, extension [.exe] *)
  val native : Mode.Js.t t

  (** Byte compilation, link with [-custom], extension [.exe] *)
  val custom : Mode.Js.t t

  (** [native] if supported, [custom] if not *)
  val native_or_custom : Context.t -> Mode.Js.t t

  val make
    :  mode:'mode
    -> ext:string
    -> ?flags:string list
    -> unit
    -> 'mode t

  val of_user_config : Context.t -> Dune_file.Executables.Link_mode.t -> Mode.Js.t t
end

(** {1 High-level functions} *)

(** Build and link one or more executables *)

val build_and_link
  :  program:Program.t
  -> linkages:Mode.Js.t Linkage.t list
  -> promote:Dune_file.Promote.t option
  -> ?link_flags:(unit, string list) Build.t
  -> Compilation_context.t
  -> unit

val build_and_link_many
  :  programs:Program.t list
  -> linkages:Mode.Js.t Linkage.t list
  -> promote:Dune_file.Promote.t option
  -> ?link_flags:(unit, string list) Build.t
  -> Compilation_context.t
  -> unit

val exe_path
  :  Compilation_context.t
  -> program:Program.t
  -> linkage:Mode.t Linkage.t
  -> Path.Build.t
