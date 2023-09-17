module T2 : sig
  type ('a, 'b) t = 'a * 'b

  val to_dyn : ('a -> Dyn.t) -> ('b -> Dyn.t) -> ('a, 'b) t -> Dyn.t
  val hash : ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int
  val equal : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

  val compare
    :  ('a -> 'a -> Ordering.t)
    -> ('b -> 'b -> Ordering.t)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> Ordering.t

  val swap : 'a * 'b -> 'b * 'a
end

module T3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  val to_dyn : ('a -> Dyn.t) -> ('b -> Dyn.t) -> ('c -> Dyn.t) -> ('a, 'b, 'c) t -> Dyn.t
  val hash : ('a -> int) -> ('b -> int) -> ('c -> int) -> ('a, 'b, 'c) t -> int

  val equal
    :  ('a -> 'a -> bool)
    -> ('b -> 'b -> bool)
    -> ('c -> 'c -> bool)
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
    -> bool
end
