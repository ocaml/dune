open Import

(** Formatting utilities for dune describe commands *)

type t =
  | Sexp
  | Csexp

(** Command line option for taking a serialisation format *)
val arg : t Term.t

(** [print_dyn t dyn] prints the dyn to stdout serialised as configured in [t] *)
val print_dyn : t -> Dyn.t -> unit
