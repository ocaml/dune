(** Compilation and linking of executables *)
open Stdune

module Program : sig
  type t =
    { name : string
    ; main_module_name : Module_name.t
    ; loc : Loc.t
    }
end

module Linkage : sig
  type t

  (** Byte compilation, exetension [.bc] *)
  val byte : t

  (** Native compilation, extension [.exe] *)
  val native : t

  (** Byte compilation, link with [-custom] or [-output-complete-exe],
      extension [.exe] *)
  val custom : Context.t -> t

  (** [native] if supported, [custom] if not *)
  val native_or_custom : Context.t -> t

  (** Javascript compilation, extension [.bc.js] *)
  val js : t

  val make : mode:Mode.t -> ext:string -> ?flags:string list -> unit -> t

  val of_user_config : Context.t -> Dune_file.Executables.Link_mode.t -> t
end

(** {1 High-level functions} *)

(** Build and link one or more executables *)

val build_and_link :
     program:Program.t
  -> linkages:Linkage.t list
  -> promote:Dune_file.Promote.t option
  -> ?link_flags:string list Build.t
  -> ?o_files:Path.t list
  -> Compilation_context.t
  -> unit

val build_and_link_many :
     programs:Program.t list
  -> linkages:Linkage.t list
  -> promote:Dune_file.Promote.t option
  -> ?link_flags:string list Build.t
  -> ?o_files:Path.t list
  -> Compilation_context.t
  -> unit

val exe_path :
     Compilation_context.t
  -> program:Program.t
  -> linkage:Linkage.t
  -> Path.Build.t
