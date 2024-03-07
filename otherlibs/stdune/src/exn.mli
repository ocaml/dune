(** Exceptions *)

type t = exn

external raise : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise : exn -> _ = "%reraise"
val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
val protectx : 'a -> f:('a -> 'b) -> finally:('a -> unit) -> 'b
val pp_uncaught : backtrace:string -> Format.formatter -> t -> unit
val pp : t -> _ Pp.t
val raise_with_backtrace : exn -> Printexc.raw_backtrace -> _
val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
