[@@@alert unstable "The API of this library is not stable and may change without notice."]
[@@@alert "-unstable"]

module Sexp = Dune_rpc.Private.Rule_plugin.Sexp

module Make
    (Fiber : Dune_rpc.Private.Fiber)
    (Chan : sig
       type t
     end)
    (_ :
       Dune_rpc.Private.Client.S with type 'a fiber := 'a Fiber.t and type chan := Chan.t) : sig
  (** Serve rule-generation requests on an established RPC channel until Dune
      disconnects. The caller supplies the fiber implementation and drives its
      event loop. Dune provides the client identity in the plugin process's
      environment.

      The callback receives the arguments of a [(plugin.NAME ...)] invocation
      and returns the stanzas that should replace it. Dune serializes requests
      sent to an individual plugin process. *)
  val run : Chan.t -> f:(Sexp.t list -> Sexp.t list Fiber.t) -> unit Fiber.t
end
