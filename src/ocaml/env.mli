open Stdune

(** Set [OCAMLRUNPARAM] to include colors *)
val with_color : Env.t -> Env.t

val caml_ld_library_path : Env.Var.t
