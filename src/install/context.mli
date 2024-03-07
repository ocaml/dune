(** Paths for the local installation directory *)

(** Return paths in [<build-dir>/install/<context-name>], which mimics the
    installation layout by creating symlinks to artifacts in the build
    directory. *)

open! Import

val install_context : Dune_engine.Build_context.t

(** Local installation directory: [<build-dir>/install/<context-name>]. *)
val dir : context:Context_name.t -> Path.Build.t

val bin_dir : context:Context_name.t -> Path.Build.t
val lib_dir : context:Context_name.t -> package:Package_name.t -> Path.Build.t
val of_path : Path.Build.t -> Context_name.t option

type analyze_path =
  | Invalid
  | Install of Context_name.t * Path.Source.t
  | Normal of Context_name.t * Path.Source.t

val analyze_path : Context_name.t -> Path.Source.t -> analyze_path
