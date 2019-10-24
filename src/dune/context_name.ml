open Stdune

module T =
  Interned.Make
    (struct
      let initial_size = 16

      let resize_policy = Interned.Conservative

      let order = Interned.Natural
    end)
    ()

include T

let default = make "default"

let build_dir t = Path.Build.relative Path.Build.root (to_string t)

let is_default = equal default

let of_string_opt name =
  if
    name = ""
    || String.is_prefix name ~prefix:"."
    || name = "log" || name = "install" || String.contains name '/'
    || String.contains name '\\'
  then
    None
  else
    Some (make name)

let error_message = sprintf "%S is an invalid context name"

let invalid_context_name (loc, s) =
  User_error.make ~loc [ Pp.text (error_message s) ]

let of_string_user_error (loc, s) =
  match of_string_opt s with
  | Some s -> Ok s
  | None -> Error (invalid_context_name (loc, s))

let of_string name =
  match of_string_opt name with
  | Some p -> p
  | None ->
    Code_error.raise "Invalid context name"
      [ ("name", Dyn.Encoder.string name) ]

let parse_string_exn s =
  match of_string_user_error s with
  | Ok s -> s
  | Error err -> raise (User_error.E err)

let decode =
  let open Dune_lang.Decoder in
  map_validate (located string) ~f:of_string_user_error

let encode t = Dune_lang.Encoder.(string (to_string t))

let to_dyn t = Dyn.Encoder.string (to_string t)

let target t ~toolchain =
  make (sprintf "%s.%s" (to_string t) (to_string toolchain))

let arg_parse s =
  match of_string_opt s with
  | Some s -> `Ok s
  | None -> `Error (error_message s)

module Infix = Comparator.Operators (T)
module Top_closure = Top_closure.Make (Set) (Monad.Id)
