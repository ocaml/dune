open! Import

(** The function used to unwrap an exception before errors are deduplicated by their
    underlying exception. It is installed by [memo.ml] so that the [Error.E] wrapper
    (defined there) is unwrapped; the default is the identity. *)
val unwrap_exn : (exn -> exn) ref

module Exn_set : Set.S with type elt := Exn_with_backtrace.t

module Collect_errors_monoid : sig
  type t =
    { exns : Exn_set.t
    ; reproducible : bool
    }

  include Monoid.S with type t := t
end

(** Restoring or computing a value can fail when the user-supplied function raises one or
    more exceptions, recorded in the [Collect_errors_monoid.t]. A computation cancelled by a
    dependency cycle is recorded as a non-reproducible [Error] whose exception set holds the
    cycle error. *)
type 'a t =
  | Ok of 'a
  | Error of Collect_errors_monoid.t

(** Get the value, or reraise the contained errors after remapping each one with [map_exn]. *)
val get_exn : 'a t -> map_exn:(exn -> exn) -> 'a Fiber.t
