(** Build-independent system variables shared by package selection and locking. *)

open Import

type t =
  { os : string option Memo.Lazy.t
  ; os_version : string option Memo.Lazy.t
  ; os_distribution : string option Memo.Lazy.t
  ; os_family : string option Memo.Lazy.t
  ; arch : string option Memo.Lazy.t
  ; sys_ocaml_version : string option Memo.Lazy.t
  }

val os : t -> Dune_lang.Pform.Var.Os.t -> string option Memo.t
val os_values : t -> Dune_lang.Pform.Var.Os.t -> Value.t list Memo.t
val poll : t
val solver_env : Dune_pkg.Solver_env.t Memo.t
val expand : t -> source:Dune_sexp.Template.Pform.t -> Pform.t -> string option Memo.t
