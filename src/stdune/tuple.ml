module T2 = struct
  type ('a, 'b) t = 'a * 'b

  let equal f g (x1, y1) (x2, y2) = f x1 x2 && g y1 y2

  let hash f g (a, b) = Hashtbl.hash (f a, g b)
end
