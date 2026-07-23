open Stdune

module T = struct
  type t =
    | Hardlink
    | Copy

  let all = [ "copy", Copy; "hardlink", Hardlink ]
  let enum = Repr.Enum.make "cache-mode" all
  let repr = Repr.Enum.repr enum
end

include T

let to_dyn = Repr.to_dyn repr
let to_string = Repr.Enum.to_string enum

let of_string s =
  match Repr.Enum.of_string enum s with
  | Some mode -> Result.Ok mode
  | None -> Result.Error (Format.sprintf "invalid cache storage mode: %s" s)
;;

include Repr.Poly (T)

(* CR-someday amokhov: Check that hard links can actually be created and, if
   not, return [Copy] instead. *)
let default () = Hardlink
