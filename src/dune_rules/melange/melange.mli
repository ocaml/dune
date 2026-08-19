open Import

module Module_system : sig
  type t =
    | ESM
    | CommonJS

  val default : t * Filename.Extension.t
  val to_string : t -> string
end

module Cli : sig
  type t =
    { package_name : string
    ; package_output : string
    ; module_name : string
    ; module_type : string
    ; stop_after_cmj : string
    }

  val of_project : Dune_project.t -> t
  val promotes_in_source : Dune_project.t -> bool
end

module Cm_kind : module type of Dune_lang.Melange.Cm_kind

val output_path : target_dir:Path.Build.t -> Path.Build.t -> Path.Build.t

module Emit : sig
  type t =
    { output_dir : Path.Build.t
    ; stanza_dir : Path.Build.t
    ; alias : Alias.Name.t
    }
end

module Source : sig
  val dir : string
end

module Install : sig
  val dir : string

  val maybe_prepend_melange_install_dir
    :  for_:Compilation_mode.t
    -> Path.Local.t option
    -> Path.Local.t option
end
