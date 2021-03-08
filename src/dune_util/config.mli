open Stdune

val dev_null : Path.t

(** [dev_null] opened in read mode *)
val dev_null_in : Unix.file_descr Lazy.t

(** [dev_null] opened in write mode *)
val dev_null_out : Unix.file_descr Lazy.t

(** Are we running inside an emacs shell? *)
val inside_emacs : bool

(** Are we running inside Dune? *)
val inside_dune : bool

(** Are we running in CI?. This checks the CI environment variable which is
    supported by travis, gitlab.*)
val inside_ci : bool
