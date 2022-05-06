open! Import
open! Dune_engine
open Stdune

include Stringlike.Make (struct
  type t = string

  let description = "external lib name"

  let module_ = "External_lib_name"

  let description_of_valid_string = None

  let hint_valid = None

  let to_string s = s

  let of_string_opt s = Some s
end)

let equal = String.equal

let compare = String.compare

let to_module_name t =
  (* TODO: correctly mangle the name to make it a valid module *)
  Module_name.of_string t
