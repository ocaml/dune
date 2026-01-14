(** Implementation of Dune shared cache. *)

module type S = Shared_intf.S

module Make (_ : sig
    val debug_shared_cache : bool
    val config : Config.t
  end) : Shared_intf.S
