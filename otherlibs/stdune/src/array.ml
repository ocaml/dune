include ArrayLabels

let equal f x y =
  let open Stdlib.Array in
  let len = length x in
  if len <> length y then false
  else
    try
      for i = 0 to len - 1 do
        if not (f (get x i) (get y i)) then raise_notrace Exit
      done;
      true
    with Exit -> false

module Immutable = struct
  type 'a t = 'a array

  let of_array a = copy a

  let to_list a = to_list a

  let of_list a = of_list a

  let map t ~f = map t ~f
end
