open Stdune

type t =
  | Hardlink
  | Copy

let equal a b =
  match a, b with
  | Hardlink, Hardlink | Copy, Copy -> true
  | _, _ -> false
;;

let all = [ "hardlink", Hardlink; "copy", Copy ]

let of_string s =
  match List.assoc all s with
  | Some mode -> Result.Ok mode
  | None -> Result.Error (Format.sprintf "invalid cache storage mode: %s" s)
;;

let to_string = function
  | Hardlink -> "hardlink"
  | Copy -> "copy"
;;

let repr =
  Repr.variant
    "cache-mode"
    [ Repr.case0 "Copy" ~test:(function
        | Copy -> true
        | Hardlink -> false)
    ; Repr.case0 "Hardlink" ~test:(function
        | Hardlink -> true
        | Copy -> false)
    ]
;;

let to_dyn = Repr.to_dyn repr

(* CR-someday amokhov: Check that hard links can actually be created and, if
   not, return [Copy] instead. *)
let default () = Hardlink
