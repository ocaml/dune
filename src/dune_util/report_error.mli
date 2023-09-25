(** Error reporting *)
open Stdune

(** Reports an error.

    Because dune doesn't stop at the first error, it might end up reporting the
    same error twice about missing libraries for instance. To avoid this, we
    keep a cache of reported errors and ignore errors that have already been
    reported.

    We cache what is actually printed to the screen. *)
val report : Exn_with_backtrace.t -> unit

val report_exception : exn -> unit
val report_backtraces : bool -> unit

(** Raised for errors that have already been reported to the user and shouldn't
    be reported again. This might happen when trying to build a dependency that
    has already failed. *)
exception Already_reported

(** Print the memo stacks of errors. *)
val print_memo_stacks : bool ref

(** Format a list of Memo stack frames into a user-friendly presentation *)
val format_memo_stack : 'a Pp.t list -> 'a Pp.t option
