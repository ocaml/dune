module type S = Map_intf.S

module Make(Key : Comparable.S) : S with type key = Key.t

val to_sexp
  :  ('a -> ('b * 'c) list)
  -> 'b Sexp.Encoder.t
  -> 'c Sexp.Encoder.t
  -> 'a Sexp.Encoder.t

val to_dyn
  :  ('a -> ('b * 'c) list)
  -> ('b -> Dyn0.t)
  -> ('c -> Dyn0.t)
  -> ('a -> Dyn0.t)
