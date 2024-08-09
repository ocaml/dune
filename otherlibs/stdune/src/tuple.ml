module T2 = struct
  type ('a, 'b) t = 'a * 'b

  let to_dyn = Dyn.pair
  let equal f g (x1, y1) (x2, y2) = f x1 x2 && g y1 y2
  let hash f g (a, b) = Poly.hash (f a, g b)

  let compare f g (a1, b1) (a2, b2) =
    match f a1 a2 with
    | (Ordering.Lt | Gt) as x -> x
    | Eq -> g b1 b2
  ;;

  let swap (x, y) = y, x
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let to_dyn = Dyn.triple
  let hash f g h (a, b, c) = Poly.hash (f a, g b, h c)
  let equal f g h (a1, b1, c1) (a2, b2, c2) = f a1 a2 && g b1 b2 && h c1 c2
end

module T4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let to_dyn = Dyn.quadruple
  let hash f g h v (a, b, c, d) = Poly.hash (f a, g b, h c, v d)

  let equal f g h v (a1, b1, c1, d1) (a2, b2, c2, d2) =
    f a1 a2 && g b1 b2 && h c1 c2 && v d1 d2
  ;;
end
