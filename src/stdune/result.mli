(** Result type *)

type ('a, 'error) t = ('a, 'error) Caml.result =
  | Ok    of 'a
  | Error of 'error

val is_ok    : _ t -> bool
val is_error : _ t -> bool

val ok_exn : ('a, exn) t -> 'a

module O : sig
  val ( >>| ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
  val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
end

val map  : ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t
val bind : ('a, 'error) t -> f:('a -> ('b, 'error) t) -> ('b, 'error) t

val map_error : ('a, 'error1) t -> f:('error1 -> 'error2) -> ('a, 'error2) t

val all : ('a, 'error) t list -> ('a list, 'error) t

val concat_map
  :  'a list
  -> f:('a -> ('b list, 'error) t)
  -> ('b list, 'error) t

(** For compatibility with some other code *)
type ('a, 'error) result = ('a, 'error) t
