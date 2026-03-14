open Import

type t =
  | Public
  | Private
  | Excluded

let to_string = function
  | Public -> "public"
  | Private -> "private"
  | Excluded -> "excluded"
;;

let to_dyn t = Dyn.string (to_string t)

let encode =
  let open Encoder in
  function
  | Public -> string "public"
  | Private -> string "private"
  | Excluded -> string "excluded"
;;

let decode =
  let open Decoder in
  plain_string (fun ~loc -> function
    | "public" -> Public
    | "private" -> Private
    | "excluded" -> Excluded
    | _ ->
      User_error.raise
        ~loc
        [ Pp.text
            "Not a valid visibility. Valid visibility is public, private, or excluded"
        ])
;;

module Map = struct
  type 'a t =
    { public : 'a
    ; private_ : 'a
    ; excluded : 'a
    }

  let make_both a = { public = a; private_ = a; excluded = a }

  let find { private_; public; excluded } = function
    | Private -> private_
    | Public -> public
    | Excluded -> excluded
  ;;
end
