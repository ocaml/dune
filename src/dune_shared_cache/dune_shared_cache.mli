(** Implementation of Dune shared cache. *)

module Make (_ : sig
    val debug_shared_cache : bool
    val config : Dune_cache.Config.t
    val upload : rule_digest:Dune_digest.t -> unit Fiber.t
    val download : rule_digest:Dune_digest.t -> unit Fiber.t
  end) : Dune_engine.Shared_cache_intf.S
