(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type kind = [ `MD5 | `SHA256 | `SHA512 ]

let default_kind = `MD5

type t = kind * string

let kind = fst
let contents = snd

(* Order by hash strength: MD5 < SHA256 < SHA512 *)
let compare_kind k l =
  match k, l with
  | `SHA512, `SHA512 | `SHA256, `SHA256 | `MD5, `MD5 -> 0
  | `MD5, _ | _, `SHA512 -> -1
  | `SHA512, _ | _, `MD5 -> 1

let compare (k,h) (l,i) =
  match compare_kind k l with
  | 0 -> String.compare h i
  | cmp -> cmp

let equal h h' = compare h h' = 0

let pfx_sep_char = '='
let pfx_sep_str = String.make 1 pfx_sep_char

let string_of_kind = function
  | `MD5 -> "md5"
  | `SHA256 -> "sha256"
  | `SHA512 -> "sha512"

let kind_of_string s = match String.lowercase_ascii s with
  | "md5" -> `MD5
  | "sha256" -> `SHA256
  | "sha512" -> `SHA512
  | _ -> invalid_arg "OpamHash.kind_of_string"

let is_hex_str len s =
  String.length s = len && OpamStd.String.is_hex s

let len = function
  | `MD5 -> 32
  | `SHA256 -> 64
  | `SHA512 -> 128

let valid kind = is_hex_str (len kind)

let is_null h =
  let count_not_zero c =
    function '0' -> c | _ -> succ c
  in
  OpamStd.String.fold_left count_not_zero 0 (contents h) <> 0

let make kind s =
  if valid kind s then kind, String.lowercase_ascii s
  else invalid_arg ("OpamHash.make_"^string_of_kind kind)

let md5 = make `MD5
let sha256 = make `SHA256
let sha512 = make `SHA512

let of_string_opt s =
  try
    let kind, s =
      match OpamStd.String.cut_at s pfx_sep_char with
      | None -> `MD5, s
      | Some (skind, s) -> kind_of_string skind, s
    in
    if valid kind s then Some (kind, String.lowercase_ascii s)
    else None
  with Invalid_argument _ -> None

let of_string s =
  match of_string_opt s with
  | Some h -> h
  | None -> invalid_arg "OpamHash.of_string"

let to_string (kind,s) =
  String.concat pfx_sep_str [string_of_kind kind; s]

let to_json s = `String (to_string s)
let of_json = function
| `String s -> of_string_opt s
| _ -> None

let to_path (kind,s) =
  [string_of_kind kind; String.sub s 0 2; s]

let sort checksums =
  List.sort (fun h h' -> compare h' h) checksums

let compute ?(kind=default_kind) file = match kind with
  | `MD5 -> md5 (Digest.to_hex (Digest.file file))
  | (`SHA256 | `SHA512) as kind -> make kind (OpamSHA.hash_file kind file)

let compute_from_string ?(kind=default_kind) str = match kind with
  | `MD5 -> md5 (Digest.to_hex (Digest.string str))
  | (`SHA256 | `SHA512) as kind -> make kind (OpamSHA.hash_string kind str)

let check_file f (kind, _ as h) = compute ~kind f = h

let mismatch f (kind, _ as h) =
  let hf = compute ~kind f in
  if hf = h then None else Some hf

module O = struct
  type _t = t
  type t = _t
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)

module Map = OpamStd.Map.Make(O)
