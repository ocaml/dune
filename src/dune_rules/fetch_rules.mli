open Import

(** Prepare source file and directory targets in the independent fetch context. *)

val context : Build_context.t
val target : Dune_pkg.Source.t -> [ `File | `Directory ] -> Path.Build.t

val local_source_contents
  :  loc:Loc.t
  -> Path.Outside_build_dir.t
  -> (Path.Local.t list * Path.Local.t list) Memo.t

val fetch
  :  target:Path.Build.t
  -> [ `File | `Directory ]
  -> Dune_pkg.Source.t
  -> Action.Full.t With_targets.t

val gen_rules
  :  dir:Path.Build.t
  -> components:Filename.t list
  -> Build_config.Gen_rules.t Memo.t
