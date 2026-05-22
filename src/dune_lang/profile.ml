open Stdune
module Stringlike = Dune_util.Stringlike

module type Stringlike = Dune_util.Stringlike

type t =
  | Dev
  | Release
  | User_defined of string

let repr =
  Repr.variant
    "profile"
    [ Repr.case0 "Dev" ~test:(function
        | Dev -> true
        | Release | User_defined _ -> false)
    ; Repr.case0 "Release" ~test:(function
        | Release -> true
        | Dev | User_defined _ -> false)
    ; Repr.case "User_defined" Repr.string ~proj:(function
        | User_defined s -> Some s
        | Dev | Release -> None)
    ]
;;

include (
  Stringlike.Make (struct
    type nonrec t = t

    let description = "profile"
    let module_ = "Profile"

    let of_string_opt p =
      let is_valid_char = function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
        | _ -> false
      in
      let is_valid_string s = s <> "" && String.for_all s ~f:is_valid_char in
      match p with
      | "dev" -> Some Dev
      | "release" -> Some Release
      | "_" -> None
      | s -> if is_valid_string s then Some (User_defined s) else None
    ;;

    let description_of_valid_string =
      Some
        (Pp.text
           "Profile names must be non-empty and can only contain letters, digits, '_' \
            and '-'. The name '_' is reserved as a wildcard.")
    ;;

    let hint_valid = None

    let to_string = function
      | Dev -> "dev"
      | Release -> "release"
      | User_defined s -> s
    ;;
  end) :
    Stringlike with type t := t)

include Repr.Poly (struct
    type nonrec t = t

    let repr = repr
  end)

let default = Dev

let is_dev = function
  | Dev -> true
  | _ -> false
;;

let is_release = function
  | Release -> true
  | _ -> false
;;

let is_inline_test = function
  | Release -> false
  | _ -> true
;;

let to_dyn = Repr.to_dyn repr
