(** Controls the verbosity of process display *)

(* Not defined in [process.ml] to avoid dependency cycles *)

(* TODO eventually separate displaying processes from running them so that
   these UI concerns live outside the engine *)

type t =
  | Quiet
  | Short
  | Verbose

val to_dyn : t -> Dyn.t
