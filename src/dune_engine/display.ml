type t =
  | Quiet
  | Short
  | Verbose

let equal a b =
  match a, b with
  | Quiet, Quiet | Short, Short | Verbose, Verbose -> true
  | _, _ -> false
;;

let to_dyn : t -> Dyn.t = function
  | Quiet -> Variant ("Quiet", [])
  | Short -> Variant ("Short", [])
  | Verbose -> Variant ("Verbose", [])
;;
