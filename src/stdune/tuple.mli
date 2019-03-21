module T2 : sig
  type ('a, 'b) t = 'a * 'b

  val hash : ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int

  val equal
    : ('a -> 'a -> bool)
    -> ('b -> 'b -> bool)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> bool
end
