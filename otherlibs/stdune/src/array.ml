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
