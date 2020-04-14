open Stdune

val report : Exn_with_backtrace.t -> unit
(** Same as {!Stdune.Report_error.report} but also print the dependency path *)
