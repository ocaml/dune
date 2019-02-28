let ( let+ ) x f = `Let (x, f)
let ( and+ ) a b = `And (a, b)

let t =
  let+ x = 1 and+ y = 2 and+ z = 3 in
  (x, y, z)

let () =
  match t with
  | `Let (`And (`And (1, 2), 3), f) ->
    assert (f ((1, 2), 3) = (1, 2, 3))
  | _ ->
    assert false
