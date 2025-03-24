type 'a t = ( :: ) of 'a * 'a list

let hd (x :: _) = x

let of_list = function
  | [] -> None
  | x :: xs -> Some (x :: xs)
;;

let to_list (x :: xs) = List.cons x xs
let map (x :: xs) ~f = f x :: List.map xs ~f
