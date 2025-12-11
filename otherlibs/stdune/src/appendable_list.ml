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

let cons x xs =
  match xs with
  | Empty -> Singleton x
  | _ -> Cons (x, xs)
;;

let to_list_rev =
  let rec loop1 acc t stack =
    match t with
    | Empty -> loop0 acc stack
    | Singleton x -> loop0 (x :: acc) stack
    | Cons (x, xs) -> loop1 (x :: acc) xs stack
    | List xs -> loop0 (List.rev_append xs acc) stack
    | Append (xs, ys) -> loop1 acc xs (ys :: stack)
    | Concat [] -> loop0 acc stack
    | Concat (x :: xs) -> loop1 acc x (Concat xs :: stack)
  and loop0 acc stack =
    match stack with
    | [] -> acc
    | t :: stack -> loop1 acc t stack
  in
  fun t -> loop1 [] t []
;;

let to_list xs = List.rev (to_list_rev xs)

let length =
  let rec loop1 len t stack =
    match t with
    | Empty -> loop0 len stack
    | Singleton _ -> loop0 (len + 1) stack
    | Cons (_, xs) -> loop1 (len + 1) xs stack
    | List xs -> loop0 (len + List.length xs) stack
    | Append (xs, ys) -> loop1 len xs (ys :: stack)
    | Concat [] -> loop0 len stack
    | Concat (x :: xs) -> loop1 len x (Concat xs :: stack)
  and loop0 len stack =
    match stack with
    | [] -> len
    | t :: stack -> loop1 len t stack
  in
  fun t -> loop1 0 t []
;;

let iter =
  let rec loop1 f t stack =
    match t with
    | Empty -> loop0 f stack
    | Singleton x ->
      f x;
      loop0 f stack
    | Cons (x, xs) ->
      f x;
      loop1 f xs stack
    | List xs ->
      List.iter xs ~f;
      loop0 f stack
    | Append (xs, ys) -> loop1 f xs (ys :: stack)
    | Concat [] -> loop0 f stack
    | Concat (x :: xs) -> loop1 f x (Concat xs :: stack)
  and loop0 f stack =
    match stack with
    | [] -> ()
    | t :: stack -> loop1 f t stack
  in
  fun t ~f -> loop1 f t []
;;

let to_immutable_array (type a) (t : a t) =
  let len = length t in
  let arr = Array.make len (Obj.magic 0 : a) in
  let i = ref 0 in
  iter t ~f:(fun x ->
    arr.(!i) <- x;
    incr i);
  assert (!i = len);
  Array.Immutable.of_array_unsafe arr
;;

let rec is_empty = function
  | List (_ :: _) | Singleton _ | Cons _ -> false
  | Append (x, y) -> is_empty x && is_empty y
  | Concat xs -> is_empty_list xs
  | List [] | Empty -> true

and is_empty_list = function
  | [] -> true
  | x :: xs -> is_empty x && is_empty_list xs
;;

let of_list = function
  | [] -> Empty
  | [ x ] -> Singleton x
  | xs -> List xs
;;

let concat = function
  | [] -> Empty
  | [ x ] -> x
  | xs -> Concat xs
;;

let rec exists t ~f =
  match t with
  | Empty -> false
  | Singleton x -> f x
  | Cons (x, xs) -> f x || exists xs ~f
  | List xs -> List.exists xs ~f
  | Append (x, y) -> exists x ~f || exists y ~f
  | Concat xs -> List.exists ~f:(exists ~f) xs
;;
