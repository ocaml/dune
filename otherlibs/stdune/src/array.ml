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

let to_list_map =
  let rec loop arr i f acc =
    if i < 0
    then acc
    else (
      let acc = f (get arr i) :: acc in
      loop arr (i - 1) f acc)
  in
  fun arr ~f -> loop arr (length arr - 1) f []
;;

let of_list_map l ~f =
  let len = List.length l in
  let l = ref l in
  init len ~f:(fun _ ->
    match !l with
    | [] -> assert false
    | x :: xs ->
      l := xs;
      f x)
;;

module Immutable = struct
  include T

  let of_array a = copy a
  let to_list_map t ~f = to_list_map t ~f
  let of_list_map t ~f = of_list_map t ~f
end
