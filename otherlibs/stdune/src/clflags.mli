(** Command line flags *)

(** Wrapper for target executables in cross-compilation: (toolchain, prog, args) *)
val target_exec : (string * string * string list) option ref

val report_errors_config : Report_errors_config.t ref

(** Stop the build upon encountering an error. *)
val stop_on_first_error : bool ref

(** Capture the output of sub-commands *)
val capture_outputs : bool ref

module Promote : sig
  type t =
    | Automatically
    | Never

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

(** explicit promotion mode is set *)
val promote : Promote.t option ref

(** Force re-running actions associated to aliases *)
val force : bool ref

(** Always show full command on error *)
val always_show_command_line : bool ref

(** The display mode *)
val display : Display.t ref

(** Whether actions are cacheable by default, default [false] *)
val can_go_in_shared_cache_default : bool ref

(** Command to use to diff files *)
val diff_command : string option ref

(** Wait for the filesystem clock to advance rather than dropping cached digest
    entries *)
val wait_for_filesystem_clock : bool ref

(** Store original source directory in dune-package metadata *)
val store_orig_src_dir : bool ref

(** Whether we are ignoring rules with [(mode promote)] *)
val ignore_promoted_rules : bool ref

(** Promote the generated [<package>.install] files to the source tree *)
val promote_install_files : bool ref

(** Print package output when building with package management *)
val debug_package_logs : bool ref

val concurrency : int ref

type on_missing_dune_project_file =
  | Error
  | Warn
  | Ignore

(** Desired behavior when dune project file is absent *)
val on_missing_dune_project_file : on_missing_dune_project_file ref
