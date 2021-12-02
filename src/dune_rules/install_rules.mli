open! Dune_engine
open Stdune

val symlink_rules :
  Super_context.t -> dir:Path.Build.t -> (Subdir_set.t * Rules.t) Memo.Build.t

(** Generate rules for [.dune-package], [META.<package-name>] files. and
    [<package-name>.install] files. *)
val gen_project_rules : Super_context.t -> Dune_project.t -> unit Memo.Build.t
