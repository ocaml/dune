module type S = Map_intf.S
module type Key = Map_intf.Key

module Make(Key : Key) : S with type key = Key.t

val to_sexp
  :  ('a -> ('b * 'c) list)
  -> 'b Sexp.Encoder.t
  -> 'c Sexp.Encoder.t
  -> 'a Sexp.Encoder.t
