include module type of S

module Make (Keys : Keys) (Monad : Monad) :
  Top_closure with type key := Keys.elt and type 'a monad := 'a Monad.t
