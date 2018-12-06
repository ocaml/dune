(** Dune files that are installed on the system *)

open! Stdune

val dune_lib_parse_sub_systems
  : ((Loc.t * Syntax.Version.t) * Dune_lang.Ast.t) Sub_system_name.Map.t
  -> Sub_system_info.t Sub_system_name.Map.t

val load : Path.t -> Sub_system_info.t Sub_system_name.Map.t
