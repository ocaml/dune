module type S = Map_intf.S

module Make(Key : Comparable.S) : S with type key = Key.t

val to_sexp
  :  ('a -> ('b * 'c) list)
  -> 'b Sexp.To_sexp.t
  -> 'c Sexp.To_sexp.t
  -> 'a Sexp.To_sexp.t
