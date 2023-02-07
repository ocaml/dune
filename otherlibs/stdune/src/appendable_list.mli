(** Appendable lists: concatenation takes O(1) time, conversion to a list takes
    O(n). *)

type 'a t

val empty : 'a t

val singleton : 'a -> 'a t

val ( @ ) : 'a t -> 'a t -> 'a t

val cons : 'a -> 'a t -> 'a t

val concat : 'a t list -> 'a t

val to_list : 'a t -> 'a list
