open! Dune_engine
open Stdune

val gen_rules : Super_context.t -> dir:Path.Build.t -> Subdir_set.t Memo.Build.t

(** Generate rules for [.dune-package] and [META.<package-name>] files. *)
val meta_and_dune_package_rules :
  Super_context.t -> Dune_project.t -> unit Memo.Build.t
