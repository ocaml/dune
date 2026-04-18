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
      (* TODO actually validate *)
      Some
        (match p with
         | "dev" -> Dev
         | "release" -> Release
         | s -> User_defined s)
    ;;

    let description_of_valid_string = None
    let hint_valid = None

    let to_string = function
      | Dev -> "dev"
      | Release -> "release"
      | User_defined s -> s
    ;;
  end) :
    Stringlike with type t := t)

let equal, _ = Repr.make_compare repr
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
