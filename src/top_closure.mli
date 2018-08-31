open! Stdune

module type Keys = sig
  type t
  type elt
  val empty : t
  val add : t -> elt -> t
  val mem : t -> elt -> bool
end

module type S = sig
  type key

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list)
    -> 'a list
    -> ('a list, 'a list) result

  (** Identical to above, but works with fibers. *)
  val top_closure_f
    :  key:('a -> key Fiber.t)
    -> deps:('a -> 'a list Fiber.t)
    -> 'a list Fiber.t
    -> ('a list, 'a list) result Fiber.t
end

module Int    : S with type key := int
module String : S with type key := string

module Make(Keys : Keys) : S with type key := Keys.elt
