[@@@alert unstable "The API of this library is not stable and may change without notice."]
[@@@alert "-unstable"]

module Sexp = Dune_rpc.Private.Rule_plugin.Sexp

(** Connect to Dune and serve rule-generation requests until Dune disconnects.

    The callback receives the arguments of a [(plugin.NAME ...)] invocation and
    returns the stanzas that should replace it. Dune serializes requests sent to
    an individual plugin process. *)
val run : (Sexp.t list -> Sexp.t list) -> unit
