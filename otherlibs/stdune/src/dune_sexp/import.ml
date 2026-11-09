let sprintf = Printf.sprintf

include Either

type ('a, 'error) result = ('a, 'error) Result.t
type ('a, 'b) either = ('a, 'b) Either.t

type ordering = Ordering.t =
  | Lt
  | Eq
  | Gt
