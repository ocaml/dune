(** Forward declarations *)

type 'a t

(** [create to_dyn] creates a forward declaration. The [to_dyn] parameter is
    used for reporting errors in [set] and [get]. *)
val create : ('a -> Dyn.t) -> 'a t

(** [set t x] sets the value that is returned by [get t] to [x]. Raises if [set]
    was already called. *)
val set : 'a t -> 'a -> unit

(** [get t] returns the [x] if [set comp x] was called. Raises if [set] has not
    been called yet. *)
val get : 'a t -> 'a

val peek : 'a t -> 'a option
