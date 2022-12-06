type 'a t = 'a list -> 'a list

let empty k = k

let singleton x k = x :: k

let to_list l = l []

let ( @ ) a b k = a (b k)

let cons x xs = singleton x @ xs

let rec concat l k =
  match l with
  | [] -> k
  | t :: l -> t (concat l k)
