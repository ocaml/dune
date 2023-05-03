(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t = string

let to_string x = x

let of_string x = x

let to_json x = `String x

let of_json = function
| `String x -> Some x
| _ -> None

let compare v w = OpamVersionCompare.compare v w
let equal v w = compare v w = 0

module O = struct
  type t = string
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)

module Map = OpamStd.Map.Make(O)

let current_raw = OpamVersionInfo.version

let current = of_string current_raw

let major v =
  try
    let i = String.index v '.' in
    of_string (String.sub v 0 i)
  with Not_found -> v

let nopatch v =
  try
    let i = String.index v '.' in
    let i = String.index_from v (i+1) '.' in
    (String.sub v 0 i)
  with Not_found ->
    let rec f i =
      if i >= String.length v then v
      else match String.get v i with
        | '0'..'9' | '.' -> f (i+1)
        | _ -> String.sub v 0 i
    in
    f 0

let current_nopatch = nopatch current_raw

let message () =
  OpamConsole.msg "\n\
    %s version %s\n\
    \n\
    Copyright (C) 2012 OCamlPro - INRIA, 2013-2015 OCamlPro\n\
    \n\
    This is free software; see the source for copying conditions.  There is NO\n\
    warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
    Sys.executable_name current_raw;
  exit 0

let gitversion = ref None

let set_git s = gitversion := Some s

let git () =
  match !gitversion with
  | None   -> None
  | Some v -> Some (of_string v)

let is_dev_version () =
  (!gitversion <> None)

let full () =
  let git_version = match git () with
    | None   -> ""
    | Some v -> Printf.sprintf " (%s)" (to_string v) in
  Printf.sprintf "%s%s" (to_string current) git_version

let magic () =
  let hash = Hashtbl.hash (full ()) in
  String.sub (Printf.sprintf "%08X" hash) 0 8
