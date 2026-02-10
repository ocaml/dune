open Import

type lib_archive_rules =
  | Not_found
  | Root
  | Rules of Rules.t

val lib_archive_rules_for_dir : dir:Path.Build.t -> lib_archive_rules Memo.t
