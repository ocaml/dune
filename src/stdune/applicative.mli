module Id : Applicative_intf.S1 with type 'a t = 'a

module Make (A : Applicative_intf.S1_base) :
  Applicative_intf.S1 with type 'a t := 'a A.t
