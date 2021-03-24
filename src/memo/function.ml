module Type = struct
  type ('a, 'b, 'f) t = Async : ('a, 'b, 'a -> 'b Fiber.t) t
end

module Info = struct
  type t =
    { name : string
    ; doc : string option
    }
end

type ('a, 'b, 'f) t =
  | Async : ('a -> 'b Fiber.t) -> ('a, 'b, 'a -> 'b Fiber.t) t

let of_type (type a b f) (t : (a, b, f) Type.t) (f : f) : (a, b, f) t =
  match t with
  | Type.Async -> Async f
