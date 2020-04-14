(** Forward declarations *)

type 'a t

val create : ('a -> Dyn.t) -> 'a t
(** [create to_dyn] creates a forward declaration. The [to_dyn] parameter is
    used for reporting errors in [set], [set_idempotent] and [get]. *)

val set : 'a t -> 'a -> unit
(** [set t x] sets the value that will be returned by [get t] to [x]. Raises if
    [set] was already called. *)

val set_idempotent : equal:('a -> 'a -> bool) -> 'a t -> 'a -> unit
(** [set_idempotent ~equal t x] sets the value that will be returned by [get t]
    to [x]. Raises if [set] or [set_idempotent] was already called with a value
    that is not [equal] to [x]. *)

val get : 'a t -> 'a
(** [get t] returns the [x] if [set comp x] was called. Raises if [set] has not
    been called yet. *)
