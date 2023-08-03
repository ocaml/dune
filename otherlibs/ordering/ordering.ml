type t =
  | Lt
  | Eq
  | Gt

let of_int n = if n < 0 then Lt else if n = 0 then Eq else Gt

let to_int = function
  | Lt -> -1
  | Eq -> 0
  | Gt -> 1
;;

let to_string = function
  | Lt -> "<"
  | Eq -> "="
  | Gt -> ">"
;;

let is_eq = function
  | Eq -> true
  | Lt | Gt -> false
;;

let min f x y =
  match f x y with
  | Eq | Lt -> x
  | Gt -> y
;;

let max f x y =
  match f x y with
  | Eq | Gt -> x
  | Lt -> y
;;

let opposite = function
  | Lt -> Gt
  | Eq -> Eq
  | Gt -> Lt
;;

let reverse f a b = opposite (f a b)

module O = struct
  let ( let= ) t f =
    match t with
    | (Lt | Gt) as result -> result
    | Eq -> f ()
  ;;
end
