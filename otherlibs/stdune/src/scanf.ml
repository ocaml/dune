module Scanf = Stdlib.Scanf

let unescaped x =
  match Scanf.unescaped x with
  | exception Scanf.Scan_failure _ -> Error ()
  | x -> Ok x
;;

exception E

let sscanf x fmt f =
  match Scanf.ksscanf x (fun _ _ -> raise_notrace E) fmt f with
  | exception E -> Error ()
  | x -> Ok x
;;
