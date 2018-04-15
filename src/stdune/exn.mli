(** Exceptions *)

type t = exn

external raise         : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise       : exn -> _ = "%reraise"

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
val protectx : 'a -> f:('a -> 'b) -> finally:('a -> unit) -> 'b

val raise_with_backtrace: exn -> Printexc.raw_backtrace -> _
