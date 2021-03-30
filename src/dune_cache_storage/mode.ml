open Stdune

type t =
  | Hardlink
  | Copy

let all = [ ("hardlink", Hardlink); ("copy", Copy) ]

let of_string s =
  match List.assoc all s with
  | Some mode -> Result.Ok mode
  | None -> Result.Error (Format.sprintf "invalid cache storage mode: %s" s)

let to_string = function
  | Hardlink -> "hardlink"
  | Copy -> "copy"

let to_dyn = function
  | Copy -> Dyn.Variant ("Copy", [])
  | Hardlink -> Dyn.Variant ("Hardlink", [])
