(** Left or right *)

type ('a, 'b) t =
  | Left of 'a
  | Right of 'b

val map : ('a, 'b) t -> l:('a -> 'c) -> r:('b -> 'c) -> 'c

val left : 'a -> ('a, _) t

val right : 'b -> (_, 'b) t

val to_dyn : ('a -> Dyn.t) -> ('b -> Dyn.t) -> ('a, 'b) t -> Dyn.t
