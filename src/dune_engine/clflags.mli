(** Command line flags *)

val report_errors_config : Report_errors_config.t ref

(** Stop the build upon encountering an error. *)
val stop_on_first_error : bool ref

(** Capture the output of sub-commands *)
val capture_outputs : bool ref

(** Always print backtraces, to help debugging dune itself *)
val debug_backtraces : bool -> unit

(** Print debug info for cached file-system operations *)
val debug_fs_cache : bool ref

(** Print debug info when loading rules in directories *)
val debug_load_dir : bool ref

(** explicit promotion mode is set *)
val promote : Dune_rpc_private.Promote_flag.t option ref

(** Force re-running actions associated to aliases *)
val force : bool ref

(** Always show full command on error *)
val always_show_command_line : bool ref

(** The display mode *)
val display : Display.t ref

(** Whether actions are cacheable by default, default [false] *)
val can_go_in_shared_cache_default : bool ref
