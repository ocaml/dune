module Int :
  Top_closure_intf.S with type key := int and type 'a monad := 'a Monad.Id.t

module String :
  Top_closure_intf.S with type key := string and type 'a monad := 'a Monad.Id.t

module Make (Keys : Top_closure_intf.Keys) (Monad : Monad_intf.S1) :
  Top_closure_intf.S with type key := Keys.elt and type 'a monad := 'a Monad.t
[@@inlined always]
