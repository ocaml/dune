type 'a t = 'a list -> 'a list

let empty k = k

let singleton x k = x :: k

let to_list l = l []

let ( @ ) a b k = a (b k)
