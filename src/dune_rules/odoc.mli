(** Odoc rules *)
open! Dune_engine

open! Stdune
open Import
open Dune_file

val setup_library_odoc_rules :
  Compilation_context.t -> Library.t -> unit Memo.Build.t

val gen_project_rules : Super_context.t -> Dune_project.t -> unit Memo.Build.t

val setup_private_library_doc_alias :
     Super_context.t
  -> scope:Scope.t
  -> dir:Stdune.Path.Build.t
  -> Dune_file.Library.t
  -> unit Memo.build

val gen_rules :
     Super_context.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.gen_rules_result Memo.Build.t
