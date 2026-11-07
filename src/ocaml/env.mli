open Stdune

(** Set [OCAMLRUNPARAM] to include colors *)
val with_color : Env.t -> Env.t

(** Set default GC parameters for compiler subprocesses, unless the user has
    supplied runtime parameters. *)
val with_gc_defaults : Env.t -> version:Version.t -> Env.t

val caml_ld_library_path : Env.Var.t
