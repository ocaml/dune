open Stdune

type t =
  | Dev
  | Release
  | User_defined of string

let of_string = function
  | "dev" ->
    Dev
  | "release" ->
    Release
  | s ->
    User_defined s

let to_string = function
  | Dev ->
    "dev"
  | Release ->
    "release"
  | User_defined s ->
    s

let default = Dev

let is_dev = function Dev -> true | _ -> false

let is_release = function Release -> true | _ -> false

let is_inline_test = function Release -> false | _ -> true

let decode =
  let open Dune_lang.Decoder in
  let+ name = string in
  of_string name

let to_dyn =
  let open Dyn.Encoder in
  function
  | Dev ->
    constr "Dyn" []
  | Release ->
    constr "Release" []
  | User_defined s ->
    constr "User_defined" [ string s ]
