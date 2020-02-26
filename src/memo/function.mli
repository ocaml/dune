module Type : sig
  type ('a, 'b, 'f) t =
    | Sync : ('a, 'b, 'a -> 'b) t
    | Async : ('a, 'b, 'a -> 'b Fiber.t) t
end

module Info : sig
  type t =
    { name : string
    ; doc : string option
    }
end

type ('a, 'b, 'f) t =
  | Sync : ('a -> 'b) -> ('a, 'b, 'a -> 'b) t
  | Async : ('a -> 'b Fiber.t) -> ('a, 'b, 'a -> 'b Fiber.t) t

val of_type : ('a, 'b, 'c) Type.t -> 'c -> ('a, 'b, 'c) t
