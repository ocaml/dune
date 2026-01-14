(** Implementation of Dune shared cache. *)

module type S = Shared_intf.S

module Make (_ : sig
    val config : Config.t
  end) : Shared_intf.S
