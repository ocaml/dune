type t = (int, string) Type_eq.t

let unreachable_code = function
  | (_ : t) -> .
;;
