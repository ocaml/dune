(** Implementation of Dune shared cache. *)
open Import

module type S = Shared_intf.S

module Make (_ : sig
    val debug_shared_cache : bool
    val config : Config.t
    val upload : rule_digest:Digest.t -> unit Fiber.t
    val download : rule_digest:Digest.t -> unit Fiber.t
  end) : Shared_intf.S
