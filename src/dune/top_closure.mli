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
  type 'a monad

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list monad)
    -> 'a list
    -> ('a list, 'a list) result monad
end

module Int    : S with type key := int and type 'a monad := 'a Monad.Id.t
module String : S with type key := string and type 'a monad := 'a Monad.Id.t

module Make(Keys : Keys)(Monad : Monad.S) : S
  with type key := Keys.elt
   and type 'a monad := 'a Monad.t
[@@inlined always]
