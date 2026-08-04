type 'a t = ( :: ) of 'a * 'a list

let hd (x :: _) = x

let last = function
  | [ t ] -> t
  | _ :: ts -> List.last ts |> Option.value_exn
;;

let destruct_last =
  let rec loop acc = function
    | [ x ] -> List.rev acc, x
    | x :: y :: xs -> loop (x :: acc) (y :: xs)
  in
  fun (xs : 'a t) -> loop [] xs
;;

let rev =
  let rec rev_append l1 (l2 :: l2s : _ t) =
    match l1 with
    | [] -> l2 :: l2s
    | a :: l -> rev_append l (a :: l2 :: l2s : _ t)
  in
  fun (x :: xs) -> rev_append xs [ x ]
;;

let of_list = function
  | [] -> None
  | x :: xs -> Some (x :: xs)
;;

let of_list_exn = function
  | [] -> Code_error.raise "Stdune.Nonempty_list.of_list_exn: empty list" []
  | x :: xs -> x :: xs
;;

let to_list (x :: xs) = List.cons x xs
let to_list_map (x :: xs) ~f = List.map (x :: xs) ~f
let map (x :: xs) ~f = f x :: List.map xs ~f

let compare (x :: xs) (y :: ys) ~compare =
  match compare x y with
  | Ordering.Eq -> List.compare xs ys ~compare
  | ordering -> ordering
;;

let concat xs (t :: ts : _ t) =
  match xs with
  | [] -> t :: ts
  | x :: xs -> x :: (xs @ (t :: ts))
;;

let ( @ ) = concat
