let sprintf = Printf.sprintf

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok of 'a
  | Error of 'error

type ('a, 'b) either = ('a, 'b) Either.t =
  | Left of 'a
  | Right of 'b

type ordering = Ordering.t =
  | Lt
  | Eq
  | Gt
