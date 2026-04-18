type t = string

let to_string t = t

let of_string_opt s =
  Option.some_if
    ((not (String.is_empty s))
     && (not (String.equal s "."))
     && not (String.exists s ~f:(Char.equal '/')))
    s
;;

let valid_description =
  Pp.text
    "Action runner names must be non-empty, may not contain '/', and may not be '.'."
;;

let error_message s = Printf.sprintf "%S is an invalid action runner name." s

let of_string s =
  match of_string_opt s with
  | Some s -> s
  | None -> Code_error.raise "Invalid Action_runner_name.t" [ "s", Dyn.string s ]
;;

let parse_string_exn (loc, s) =
  match of_string_opt s with
  | Some s -> s
  | None -> User_error.raise ~loc [ Pp.text (error_message s); valid_description ]
;;

let repr = Repr.view Repr.string ~to_:to_string
