(** Error reporting *)
open Stdune

(** Reports an error.

    Because dune doesn't stop at the first error, it might end up reporting the
    same error twice about missing libraries for instance. To avoid this, we
    keep a cache of reported errors and ignore errors that have already been
    reported.

    We cache what is actually printed to the screen.

    [extra] is the extra material that is printed between the backtrace and the
    hint. *)
val report :
     ?extra:(Loc.t option -> User_message.Style.t Pp.t option)
  -> Exn_with_backtrace.t
  -> unit

val report_backtraces : bool -> unit

(** Raised for errors that have already been reported to the user and shouldn't
    be reported again. This might happen when trying to build a dependency that
    has already failed. *)
exception Already_reported

(** Clear the list of already reported errors. *)
val clear_reported : unit -> unit

(**/**)

val ppf : Format.formatter
