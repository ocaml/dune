module T2 = struct
  type ('a, 'b) t = 'a * 'b

  let to_dyn = Dyn.Encoder.pair

  let equal f g (x1, y1) (x2, y2) = f x1 x2 && g y1 y2

  let hash f g (a, b) = Hashtbl.hash (f a, g b)

  let compare f g (a1, b1) (a2, b2) =
    match f a1 a2 with
    | (Ordering.Lt | Gt) as x -> x
    | Eq -> g b1 b2

  let swap (x, y) = (y, x)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let to_dyn = Dyn.Encoder.triple

  let hash f g h (a, b, c) = Hashtbl.hash (f a, g b, h c)
end
