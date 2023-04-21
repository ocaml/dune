(**************************************************************************)
(*                                                                        *)
(*    Copyright 2022 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t = unit

let compare _a _b = 1
let equal _a _b = false

let of_string_opt _s = None

let of_string s =
  match of_string_opt s with
  | None -> invalid_arg "OpamSWHSWH_ID.of_string"
  | Some i -> i

let to_string _ = ""

let to_json s = `String (to_string s)
let of_json = function
  | `String s -> of_string_opt s
  | _ -> None

let hash _ = ""

module O = struct
  type nonrec t = t
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)
module Map = OpamStd.Map.Make(O)


(** Url handling *)

let is_valid _url = false

let of_url _url = None

let to_url _swh = failwith "stubbed out"

let compute _dir = None
