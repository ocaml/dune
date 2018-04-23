(** Exceptions *)

(** An programming error, that should be reported upstream. The error message
    shouldn't try to be developer friendly rather than user friendly. *)
exception Code_error of Usexp.t

val code_error : string -> (string * Usexp.t) list -> _

type t = exn

external raise         : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise       : exn -> _ = "%reraise"

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
val protectx : 'a -> f:('a -> 'b) -> finally:('a -> unit) -> 'b

val raise_with_backtrace: exn -> Printexc.raw_backtrace -> _
