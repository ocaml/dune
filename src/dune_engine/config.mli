(** Configuration parameters *)

open! Import

(** Local installation directory *)
val local_install_dir : context:Context_name.t -> Path.Build.t

val local_install_lib_root : context:Context_name.t -> Path.Build.t

val local_install_bin_dir : context:Context_name.t -> Path.Build.t

val local_install_man_dir : context:Context_name.t -> Path.Build.t

val local_install_lib_dir :
  context:Context_name.t -> package:Package.Name.t -> Path.Build.t

val dev_null : Path.t

(** [dev_null] opened in read mode *)
val dev_null_in : Unix.file_descr Lazy.t

(** [dev_null] opened in write mode *)
val dev_null_out : Unix.file_descr Lazy.t

(** When this file is present in a directory dune will delete nothing in it if
    it knows to generate this file. *)
val dune_keep_fname : string

(** Are we running inside an emacs shell? *)
val inside_emacs : bool

(** Are we running inside Dune? *)
val inside_dune : bool

(** Are we running in CI?. This checks the CI environment variable which is
    supported by travis, gitlab.*)
val inside_ci : bool

val show_full_command_on_error : unit -> bool
