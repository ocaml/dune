open Stdune

type t = Digest.t

let to_string = Digest.to_string

let of_string s =
  match Digest.from_hex s with
  | Some d -> Result.Ok d
  | None -> Result.Error (Printf.sprintf "invalid key: %s" s)
