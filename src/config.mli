(** Configuration parameters *)

open! Import

(** Local installation directory *)
val local_install_dir : context:string -> Path.t

val local_install_bin_dir : context:string -> Path.t
val local_install_lib_dir : context:string -> package:string -> Path.t
