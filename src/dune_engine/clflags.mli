(** Command line flags *)

val report_errors_config : Report_errors_config.t ref

(** Debug the findlib implementation *)
val debug_findlib : bool ref

(** Capture the output of sub-commands *)
val capture_outputs : bool ref

(** Always print backtraces, to help debugging dune itself *)
val debug_backtraces : bool -> unit

(** Print debug info about artifact substitution *)
val debug_artifact_substitution : bool ref

(** Print debug info for cached digests *)
val debug_digests : bool ref

(** Print debug info for cached file-system operations *)
val debug_fs_cache : bool ref

(** Print debug info when loading rules in directories *)
val debug_load_dir : bool ref

(** Wait for the filesystem clock to advance rather than dropping cached digest
    entries *)
val wait_for_filesystem_clock : bool ref

(** Command to use to diff things *)
val diff_command : string option ref

module Promote : sig
  type t =
    | Automatically
    | Never
end

(** explicit promotion mode is set *)
val promote : Promote.t option ref

(** Force re-running actions associated to aliases *)
val force : bool ref

(** Do not print "Entering directory" messages *)
val no_print_directory : bool ref

(** Store original source directory in dune-package metadata *)
val store_orig_src_dir : bool ref

(** Always show full command on error *)
val always_show_command_line : bool ref

(** Promote the generated [<package>.install] files to the source tree *)
val promote_install_files : bool ref

(** Whether we are ignoring rules with [(mode promote)] *)
val ignore_promoted_rules : bool ref

type on_missing_dune_project_file =
  | Error
  | Warn
  | Ignore

(** Desired behavior when dune project file is absent *)
val on_missing_dune_project_file : on_missing_dune_project_file ref
