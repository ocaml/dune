(** Command line flags *)

(** Print dependency path in case of error *)
val debug_dep_path : bool ref

(** Debug the findlib implementation *)
val debug_findlib : bool ref

(** The command line for "Hint: try: dune external-lib-deps ..." *)
val external_lib_deps_hint : string list ref

val external_lib_deps_mode : bool ref

(** Capture the output of sub-commands *)
val capture_outputs : bool ref

(** Always print backtraces, to help debugging dune itself *)
val debug_backtraces : bool -> unit

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

(** Instead of terminating build after completion, watch for changes *)
val watch : bool ref

(** Do not print "Entering directory" messages *)
val no_print_directory : bool ref

(** Store original source directory in dune-package metadata *)
val store_orig_src_dir : bool ref

(** Always show full command on error *)
val always_show_command_line : bool ref

(** Promote the generated [<package>.install] files to the source tree *)
val promote_install_files : bool ref

(** Wether we are ignorimg rules with [(mode promote)] *)
val ignore_promoted_rules : bool ref
