open Import

type lib_archive_rules =
  | Not_found
  | Root
  | Rules of Rules.t

val build_only_sub_dirs_for_dir_status
  :  Super_context.t Memo.t
  -> Dir_status.t
  -> dir:Path.Build.t
  -> Build_config.Gen_rules.Build_only_sub_dirs.t Memo.t

val lib_archive_rules_for_dir : dir:Path.Build.t -> lib_archive_rules Memo.t
