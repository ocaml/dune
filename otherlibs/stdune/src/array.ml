module T = struct
  include ArrayLabels

  let equal f x y =
    let open Stdlib.Array in
    let len = length x in
    if len <> length y
    then false
    else (
      try
        for i = 0 to len - 1 do
          if not (f (get x i) (get y i)) then raise_notrace Exit
        done;
        true
      with
      | Exit -> false)
  ;;

  let to_dyn f t = Dyn.array f t
  let map t ~f = map t ~f
  let fold_right t ~f ~init = fold_right t ~f ~init
  let exists t ~f = exists t ~f
end

include T

module Immutable = struct
  include T

  let of_array a = copy a
end
