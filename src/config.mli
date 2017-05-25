(** Configuration parameters *)

open! Import

(** Local installation directory *)
val local_install_dir : context:string -> Path.t

val local_install_bin_dir : context:string -> Path.t
val local_install_man_dir : context:string -> Path.t
val local_install_lib_dir : context:string -> package:string -> Path.t

val dev_null : Path.t

(** When this file is present in a directory jbuilder will delete
    nothing in it if it knows to generate this file. *)
val jbuilder_keep_fname : string
