module type S = Set_intf.S

module Make(Elt : Comparable.S) : S with type elt = Elt.t

val to_sexp
  :  ('a -> 'b list)
  -> 'b Sexp.To_sexp.t
  -> 'a Sexp.To_sexp.t
