(** Forward declarations *)

type 'a t

(** [create ()] creates a forward declaration. *)
val create : ('a -> Dyn.t) -> 'a t

(** [set t x] set's the value that is returned by [get t] to [x]. Raise if
    [set] was already called *)
val set : 'a t -> 'a -> unit

(** [get t] returns the [x] if [set comp x] was called. Raise if [set] has not
    been called yet. *)
val get : 'a t -> 'a

val peek : 'a t -> 'a option
