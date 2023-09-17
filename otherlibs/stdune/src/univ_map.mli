(** Universal maps *)

module type S = Univ_map_intf.S

module Make
    (Info : sig
       type 'a t
     end)
    () : sig
  module Key : Univ_map_intf.Key with type 'a info = 'a Info.t
  include S with module Key := Key
end

module Key : sig
  include Univ_map_intf.Key

  val create : name:string -> ('a -> Dyn.t) -> 'a t
end

include S with module Key := Key

val to_dyn : t -> Dyn.t
