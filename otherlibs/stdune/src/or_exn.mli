(** Either a value or an exception *)

type 'a t = ('a, exn) Result.t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val hash : ('a -> int) -> 'a t -> int
val to_dyn : ('a -> Dyn.t) -> 'a t Dyn.builder

include Monad_intf.S with type 'a t := 'a t
