(** Store original source directory in dune-package metadata *)
val store_orig_src_dir : bool ref

(** Whether we are ignoring rules with [(mode promote)] *)
val ignore_promoted_rules : bool ref

(** Promote the generated [<package>.install] files to the source tree *)
val promote_install_files : bool ref

(** Re-exported form [Dune_engine] *)
val display : Dune_engine.Display.t ref

(** Re-exported form [Dune_engine] *)
val capture_outputs : bool ref

(** Print package output when building with package management *)
val debug_package_logs : bool ref

(** Whether we are ignoring "dune.lock/". *)
val ignore_lock_dir : bool ref

val concurrency : int ref
