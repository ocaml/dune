open! Stdune
(** Error reporting *)

(** Captures the backtrace and report an error.

    Because jbuilder doesn't stop at the first error, it might end up
    reporting the same error twice about missing libraries for
    instance. To avoid this, we keep a cache of reported errors and
    ignore errors that have already been reported.

    We cache what is actually printed to the screen.  *)
val report : exn -> unit

type printer

val make_printer :
  ?backtrace:bool ->
  ?hint:string ->
  ?loc:Loc.t ->
  (Format.formatter -> unit) ->
  printer

(** Register an error reporter. *)
val register : (exn -> printer option) -> unit

(**/**)
val map_fname : (string -> string) ref
