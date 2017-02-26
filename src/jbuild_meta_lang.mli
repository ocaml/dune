(** Meta Jbuild language *)

open! Import

val expand : Sexp.Ast.t list -> context:Context.t -> Sexp.Ast.t list
