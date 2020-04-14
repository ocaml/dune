(** Command line flags *)

val debug_dep_path : bool ref
(** Print dependency path in case of error *)

val debug_findlib : bool ref
(** Debug the findlib implementation *)

val external_lib_deps_hint : string list ref
(** The command line for "Hint: try: dune external-lib-deps ..." *)

val capture_outputs : bool ref
(** Capture the output of sub-commands *)

val debug_backtraces : bool -> unit
(** Always print backtraces, to help debugging dune itself *)

val diff_command : string option ref
(** Command to use to diff things *)

module Promote : sig
  type t =
    | Automatically
    | Never
end

val promote : Promote.t option ref
(** explicit promotion mode is set *)

val force : bool ref
(** Force re-running actions associated to aliases *)

val watch : bool ref
(** Instead of terminating build after completion, watch for changes *)

val no_print_directory : bool ref
(** Do not print "Entering directory" messages *)

val store_orig_src_dir : bool ref
(** Store original source directory in dune-package metadata *)

val always_show_command_line : bool ref
(** Always show full command on error *)

val promote_install_files : bool ref
(** Promote the generated [<package>.install] files to the source tree *)

val ignore_promoted_rules : bool ref
(** Wether we are ignorimg rules with [(mode promote)] *)
