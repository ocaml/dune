type 'a t =
  | Empty
  | Singleton of 'a
  | Cons of 'a * 'a t
  | List of 'a list
  | Append of 'a t * 'a t
  | Concat of 'a t list

let empty = Empty
let singleton x = Singleton x

let ( @ ) a b =
  match a, b with
  | Empty, _ -> b
  | _, Empty -> a
  | Singleton a, _ -> Cons (a, b)
  | _, _ -> Append (a, b)
;;

let cons x xs = Cons (x, xs)

let to_list_rev =
  let rec loop acc stack =
    match stack with
    | [] -> acc
    | t :: stack ->
      (match t with
       | Empty -> loop acc stack
       | Singleton x -> loop (x :: acc) stack
       | Cons (x, xs) -> loop (x :: acc) (xs :: stack)
       | List xs -> loop (List.rev_append xs acc) stack
       | Append (xs, ys) -> loop acc (xs :: ys :: stack)
       | Concat [] -> loop acc stack
       | Concat (x :: xs) -> loop acc (x :: Concat xs :: stack))
  in
  fun t -> loop [] [ t ]
;;

let to_list xs = List.rev (to_list_rev xs)

let rec is_empty = function
  | List (_ :: _) | Singleton _ | Cons _ -> false
  | Append (x, y) -> is_empty x && is_empty y
  | Concat xs -> is_empty_list xs
  | List [] | Empty -> true

and is_empty_list = function
  | [] -> true
  | x :: xs -> is_empty x && is_empty_list xs
;;

let concat list = Concat list
let of_list x = List x
