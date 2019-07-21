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
  type t

  module Js : sig
    type linkage
    type t =
      | Js
      | NonJs of linkage
  end with type linkage := t

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

  val of_user_config : Context.t -> Dune_file.Executables.Link_mode.t -> Js.t
end

(** {1 High-level functions} *)

(** Build and link one or more executables *)

val build_and_link
  :  program:Program.t
  -> linkages:Linkage.Js.t list
  -> promote:Dune_file.Promote.t option
  -> ?link_flags:(unit, string list) Build.t
  -> Compilation_context.t
  -> unit

val build_and_link_many
  :  programs:Program.t list
  -> linkages:Linkage.Js.t list
  -> promote:Dune_file.Promote.t option
  -> ?link_flags:(unit, string list) Build.t
  -> Compilation_context.t
  -> unit

val exe_path
  :  Compilation_context.t
  -> program:Program.t
  -> linkage:Linkage.t
  -> Path.Build.t
