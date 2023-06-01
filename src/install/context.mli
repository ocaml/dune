(** Paths for the local installation directory *)

(** Return paths in [<build-dir>/install/<context-name>], which mimics the
    installation layout by creating symlinks to artifacts in the build
    directory. *)

open! Import

(** Local installation directory: [<build-dir>/install/<context-name>]. *)
val dir : context:Context_name.t -> Path.Build.t

val lib_root : context:Context_name.t -> Path.Build.t

val bin_dir : context:Context_name.t -> Path.Build.t

val man_dir : context:Context_name.t -> Path.Build.t

val lib_dir : context:Context_name.t -> package:Package_name.t -> Path.Build.t
