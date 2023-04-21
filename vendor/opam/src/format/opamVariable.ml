(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

include OpamStd.AbstractString

type variable = t

type variable_contents =
  | B of bool
  | S of string
  | L of string list

let string_of_variable_contents = function
  | B b -> string_of_bool b
  | S s -> s
  | L l -> String.concat " " l

let string str = S str

let bool b = B b

let int i = string (string_of_int i)

let dirname dir = string (OpamFilename.Dir.to_string dir)

module Full = struct

  type scope =
    | Global
    | Self
    | Package of OpamPackage.Name.t

  type t = {
    scope: scope;
    variable: variable;
  }

  let variable t = t.variable
  let scope t = t.scope
  let package ?self t = match t.scope with
    | Package p -> Some p
    | Self -> self
    | Global -> None

  let create package variable =
    let scope =
      if OpamPackage.Name.to_string package = "_" then Self
      else Package package
    in
    { scope; variable }

  (* Read the variables overridden through the environment *)
  let read_from_env v =
    let var_str = to_string (variable v) in
    let undash = OpamStd.String.map (function '-' -> '_' | c -> c) in
    let var_hook =
      match package v with
      | Some n ->
        Printf.sprintf "%s_%s" (undash (OpamPackage.Name.to_string n))
          (undash var_str)
      | None -> undash var_str
    in
    try match OpamStd.Env.get ("OPAMVAR_" ^ var_hook) with
      | "true"  | "1" -> Some (bool true)
      | "false" | "0" -> Some (bool false)
      | s             -> Some (string s)
    with Not_found -> None

  let global variable =
    { scope = Global; variable }

  let self variable =
    { scope = Self; variable }

  let is_global variable = match variable.scope with
    | Global -> true
    | Self | Package _ -> false

  let of_string s =
    match OpamStd.String.rcut_at s ':' with
    | None -> global (of_string s)
    | Some ("_",v) ->
      { scope = Self; variable = of_string v }
    | Some (p,v) ->
      create (OpamPackage.Name.of_string p) (of_string v)

  let to_string t =
    let prefix =
      match t.scope with
      | Global -> ""
      | Self -> "_:"
      | Package p -> OpamPackage.Name.to_string p ^ ":"
    in
    prefix ^ to_string t.variable

  let to_json x =
    `String (to_string x)

  let of_json = function
    | `String s -> (try Some (of_string s) with _ -> None)
    | _ -> None

  let compare {scope; variable} fv =
    match scope, fv.scope with
    | Global, Global | Self, Self ->
      String.compare variable fv.variable
    | Package n, Package m ->
      let package = OpamPackage.Name.compare n m in
      if package <> 0 then package else
        String.compare variable fv.variable
    | Global, _ | _, Self -> 1
    | Self, _ | _, Global -> -1

  let equal f g = compare f g = 0

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
    let to_json = to_json
    let of_json = of_json
  end

  module Set = OpamStd.Set.Make(O)

  module Map = OpamStd.Map.Make(O)

end
