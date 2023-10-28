type 'a t =
  | Empty
  | Singleton of 'a
  | Cons of 'a * 'a t
  | List of 'a * 'a * 'a list
  | Append of 'a t * 'a t
  | Concat of 'a t list

let empty = Empty
let singleton x = Singleton x

let ( @ ) a b =
  match a, b with
  | Empty, _ -> b
  | _, Empty -> a
  | Singleton a, Singleton b -> List (a, b, [])
  | Singleton a, _ -> Cons (a, b)
  | _, _ -> Append (a, b)
;;

let cons x xs =
  match xs with
  | Empty -> Singleton x
  | List (y, z, rest) -> List (x, y, z :: rest)
  | _ -> Cons (x, xs)
;;

let to_list_rev =
  let rec loop acc stack =
    match stack with
    | [] -> acc
    | t :: stack ->
      (match t with
       | Empty -> loop acc stack
       | Singleton x -> loop (x :: acc) stack
       | Cons (x, xs) -> loop (x :: acc) (xs :: stack)
       | List (x, y, xs) -> loop (List.rev_append xs (y :: x :: acc)) stack
       | Append (xs, ys) -> loop acc (xs :: ys :: stack)
       | Concat [] -> loop acc stack
       | Concat (x :: xs) -> loop acc (x :: Concat xs :: stack))
  in
  fun t -> loop [] [ t ]
;;

let to_list xs = List.rev (to_list_rev xs)

let is_empty = function
  | Empty -> true
  | _ -> false
;;

let rec concat = function
  | [] -> Empty
  | [ x ] -> if is_empty x then Empty else x
  | x :: xs as list -> if is_empty x then concat xs else Concat list
;;

let of_list = function
  | [] -> Empty
  | [ x ] -> Singleton x
  | x :: y :: xs -> List (x, y, xs)
;;

let rec of_list_concat = function
  | [] -> Empty
  | x :: xs as list -> if is_empty x then of_list_concat xs else Concat list
;;
