(** The main logic for the runtime versioning protocol for the Dune RPC. For a
    high-level explanation and rationale, see [doc/dev/rpc-versioning.ml]. *)

module Make (Fiber : Fiber_intf.S) : Versioned_intf.S with type 'a fiber := 'a Fiber.t
