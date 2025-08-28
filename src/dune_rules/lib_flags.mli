open Import

(** Operation on list of libraries and modules *)
module Lib_and_module : sig
  type t =
    | Lib of Lib.t
    | Module of Path.t Obj_dir.t * Module.t

  module L : sig
    type nonrec t = t list

    val link_flags
      :  Super_context.t
      -> t
      -> lib_config:Lib_config.t
      -> mode:Link_mode.t
      -> _ Command.Args.t
  end
end

(** Operations on list of libraries *)
module L : sig
  type nonrec t = Lib.t list

  val to_iflags : Path.Set.t -> _ Command.Args.t

  type flag =
    | Hidden
    | Include

  type flags := flag Path.Map.t

  val include_only : flags -> Path.t list
  val to_flags : flags -> _ Command.Args.t
  val include_paths : ?project:Dune_project.t -> t -> Lib_mode.t -> Lib_config.t -> flags

  val include_flags
    :  ?project:Dune_project.t
    -> direct_libs:t
    -> hidden_libs:t
    -> Lib_mode.t
    -> Lib_config.t
    -> _ Command.Args.t

  val melange_emission_include_flags
    :  ?project:Dune_project.t
    -> t
    -> Lib_config.t
    -> _ Command.Args.t

  val c_include_flags : t -> Super_context.t -> _ Command.Args.t
  val toplevel_ld_paths : t -> Lib_config.t -> Path.Set.t
  val toplevel_include_paths : t -> Lib_config.t -> flags
end

(** The list of files that will be read by the compiler when linking an
    executable against this library *)
val link_deps
  :  Super_context.t
  -> Lib.t
  -> Link_mode.t
  -> Lib_config.t
  -> Path.t list Memo.t
