include sig
  [@@@warning "-33"]
  (* This open is unused with OCaml >= 4.03 since the stdlib defines a result type *)
  open Result_compat
  open Pervasives

  (** The result type.

      It is equal to the result type defined by the standard library for OCaml >= 4.03.
  *)
  type ('a, 'error) t = ('a, 'error) result =
    | Ok    of 'a
    | Error of 'error
end

val is_ok    : _ t -> bool
val is_error : _ t -> bool

module O : sig
  val ( >>| ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
  val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
end

val map  : ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t
val bind : ('a, 'error) t -> f:('a -> ('b, 'error) t) -> ('b, 'error) t

val map_error : ('a, 'error1) t -> f:('error1 -> 'error2) -> ('a, 'error2) t

(** For compatibility with some other code *)
type ('a, 'error) result = ('a, 'error) t
