open Stdune

module Name =
  Interned.Make
    (struct
      let initial_size = 1024

      let resize_policy = Interned.Greedy

      let order = Interned.Fast
    end)
    ()

module Type = struct
  type ('a, 'b, 'f) t =
    | Sync : ('a, 'b, 'a -> 'b) t
    | Async : ('a, 'b, 'a -> 'b Fiber.t) t
end

module Info = struct
  type t =
    { name : Name.t
    ; doc : string
    }
end

type ('a, 'b, 'f) t =
  | Sync : ('a -> 'b) -> ('a, 'b, 'a -> 'b) t
  | Async : ('a -> 'b Fiber.t) -> ('a, 'b, 'a -> 'b Fiber.t) t

let of_type (type a b f) (t : (a, b, f) Type.t) (f : f) : (a, b, f) t =
  match t with
  | Type.Sync -> Sync f
  | Type.Async -> Async f
