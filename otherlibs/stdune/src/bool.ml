let compare x y =
  match x, y with
  | true, true | false, false -> Ordering.Eq
  | true, false -> Gt
  | false, true -> Lt
;;

include Comparator.Operators (struct
    type nonrec t = bool

    let compare = compare
  end)

let to_string = string_of_bool
let of_string s = bool_of_string_opt s
let to_dyn t = Dyn.Bool t

let[@inline always] hash = function
  | true -> 1
  | false -> 0
;;
