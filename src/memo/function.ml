open Stdune

module Type = struct
  type ('a, 'b, 'f) t =
    | Sync : ('a, 'b, 'a -> 'b) t
    | Async : ('a, 'b, 'a -> 'b Fiber.t) t
end

module Info = struct
  type t =
    | Named of
        { name : string
        ; doc : string option
        }
    | Loc of Loc.t

  let of_loc loc = Loc loc

  let name = function
    | Named { name ; doc = _ } -> Some name
    | Loc _ -> None

  let doc = function
    | Named { doc ; name = _ } -> doc
    | Loc _ -> None

  let named ?doc ~name () = Named { name ; doc }

  let to_dyn = function
    | Named { name ; doc = _ } ->
      Dyn.Tuple [String name]
    | Loc loc -> Loc.to_dyn_hum loc
end

type ('a, 'b, 'f) t =
  | Sync : ('a -> 'b) -> ('a, 'b, 'a -> 'b) t
  | Async : ('a -> 'b Fiber.t) -> ('a, 'b, 'a -> 'b Fiber.t) t

let of_type (type a b f) (t : (a, b, f) Type.t) (f : f) : (a, b, f) t =
  match t with
  | Type.Sync -> Sync f
  | Type.Async -> Async f
