(** Appendable lists: concatenation takes O(1) time, conversion to a list takes
    O(n). *)

type 'a t

val empty : 'a t
val singleton : 'a -> 'a t
val ( @ ) : 'a t -> 'a t -> 'a t
val cons : 'a -> 'a t -> 'a t
val to_list : 'a t -> 'a list
val to_list_rev : 'a t -> 'a list
val of_list : 'a list -> 'a t
val concat : 'a t list -> 'a t

(** The current implementation is slow, don't use it on a hot path. *)
val is_empty : _ t -> bool
