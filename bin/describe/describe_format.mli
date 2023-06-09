open Import

(** Formatting utilities for dune describe commands *)

type t =
  | Sexp
  | Csexp

val arg : t Term.t

val print_dyn : t -> Dyn.t -> unit
