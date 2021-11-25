(** Universal maps *)

module type S = Univ_map_intf.S

include S

module Make () : S
