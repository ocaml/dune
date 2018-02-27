(** Error reporting *)

(** Captures the backtrace and report an error.

    Because jbuilder doesn't stop at the first error, it might end up
    reporting the same error twice about missing libraries for
    instance. To avoid this, we keep a cache of reported errors and
    ignore errors that have already been reported.

    We cache what is actually printed to the screen.  *)
val report : exn -> unit

type printer =
  { loc       : Loc.t option
  ; pp        : Format.formatter -> unit
  ; hint      : string option
  ; backtrace : bool
  }

(** Register an error reporter. *)
val register : (exn -> printer option) -> unit

(**/**)
val map_fname : (string -> string) ref
