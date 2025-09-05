module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  module O : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type Keys = sig
  type t
  type elt

  val empty : t
  val add : t -> elt -> t
  val mem : t -> elt -> bool
end

module type Top_closure = sig
  type key
  type 'a monad

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list monad)
    -> 'a list
    -> ('a list, 'a list) result monad
end
