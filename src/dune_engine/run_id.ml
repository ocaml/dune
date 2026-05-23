type t =
  | Batch
  | Watch of int

let to_int = function
  | Batch -> 0
  | Watch n -> n
;;
