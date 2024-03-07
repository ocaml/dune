type t =
  | Quiet
  | Short
  | Verbose

let to_dyn : t -> Dyn.t = function
  | Quiet -> Variant ("Quiet", [])
  | Short -> Variant ("Short", [])
  | Verbose -> Variant ("Verbose", [])
;;
