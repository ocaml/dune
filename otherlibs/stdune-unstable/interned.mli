(** Interned strings *)

type resize_policy =
  | Conservative
  | Greedy

type order =
  | Natural
  | Fast

module type Settings = sig
  val initial_size : int

  val resize_policy : resize_policy

  val order : order
end

module Make (R : Settings) () : Interned_intf.S

module No_interning (R : Settings) () : Interned_intf.S
