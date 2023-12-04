open Stdune

type rebuild_trigger =
  | Eager
  | Passive

type t =
  | No
  | Yes of rebuild_trigger

let all = [ No; Yes Eager; Yes Passive ]

let to_string = function
  | No -> "no"
  | Yes Eager -> "eager"
  | Yes Passive -> "passive"
;;

let of_string s =
  match s with
  | "no" -> Ok No
  | "eager" -> Ok (Yes Eager)
  | "passive" -> Ok (Yes Passive)
  | s ->
    Error
      (Printf.sprintf
         "invalid watch mode %S, should be one of: %s"
         s
         (String.concat ~sep:", " (List.map ~f:to_string all)))
;;
