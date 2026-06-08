open Import

module Module_system : sig
  type t =
    | ESM
    | CommonJS

  val default : t * Filename.Extension.t
  val to_string : t -> string
end

module Cm_kind : module type of Dune_lang.Melange.Cm_kind

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
