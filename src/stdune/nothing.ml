type t = (int, string) Type_eq.t

let unreachable_code (t : t) = match t with
  | _ -> .
