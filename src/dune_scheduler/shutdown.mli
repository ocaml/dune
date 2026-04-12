open Stdune

module Reason : sig
  type t =
    | Requested
    | Timeout
    | Signal of Signal.t

  module Set : Set.S with type elt = t
end

(** Raised when [go] terminates due to the user requesting a shutdown via
    rpc or raising a signal. The caller needs to know about this to set the
    exit code correctly *)
exception E of Reason.t
