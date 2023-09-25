open Stdune
module Stringlike = Dune_util.Stringlike

module type Stringlike = Dune_util.Stringlike

type t =
  | Dev
  | Release
  | User_defined of string

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

let equal x y =
  match x, y with
  | Dev, Dev -> true
  | Release, Release -> true
  | User_defined x, User_defined y -> String.equal x y
  | _, _ -> false
;;

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

let to_dyn =
  let open Dyn in
  function
  | Dev -> variant "Dev" []
  | Release -> variant "Release" []
  | User_defined s -> variant "User_defined" [ string s ]
;;
