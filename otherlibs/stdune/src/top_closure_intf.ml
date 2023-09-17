module type Keys = sig
  type t
  type elt

  val empty : t
  val add : t -> elt -> t
  val mem : t -> elt -> bool
end

module type S = sig
  type key
  type 'a monad

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list monad)
    -> 'a list
    -> ('a list, 'a list) result monad
end
