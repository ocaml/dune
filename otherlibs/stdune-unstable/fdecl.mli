(** Forward declarations *)

type 'a t

(** [create to_dyn] creates a forward declaration. The [to_dyn] parameter is
    used for reporting errors in [set], [set_idempotent] and [get]. *)
val create : ('a -> Dyn.t) -> 'a t

(** [set t x] sets the value that will be returned by [get t] to [x]. Raises if
    [set] was already called. *)
val set : 'a t -> 'a -> unit

(** [get t] returns the [x] if [set comp x] was called. Raises if [set] has not
    been called yet. *)
val get : 'a t -> 'a

val to_dyn : 'a t -> Dyn.t
