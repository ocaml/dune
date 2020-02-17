open Stdune

module T = struct
  type t = string

  let compare = Poly.compare

  let to_dyn = Dyn.Encoder.string
end

include T

let encode = Dune_lang.Encoder.string

let add_suffix = ( ^ )

let is_valid_module_name name =
  match name with
  | "" -> false
  | s -> (
    try
      ( match s.[0] with
      | 'A' .. 'Z'
      | 'a' .. 'z' ->
        ()
      | _ -> raise_notrace Exit );
      String.iter s ~f:(function
        | 'A' .. 'Z'
        | 'a' .. 'z'
        | '0' .. '9'
        | '\''
        | '_' ->
          ()
        | _ -> raise_notrace Exit);
      true
    with Exit -> false )

let parse_string s =
  if is_valid_module_name s then
    Some (String.capitalize s)
  else
    None

let of_string s =
  match parse_string s with
  | Some s -> s
  | None ->
    Code_error.raise "Module_name.of_string: invalid name"
      [ ("s", Dyn.Encoder.string s) ]

let to_string x = x

let uncapitalize = String.uncapitalize

let pp_quote fmt x = Format.fprintf fmt "%S" x

module Set = struct
  include String.Set

  let to_dyn t = Dyn.Set (List.map ~f:(fun s -> Dyn.String s) (to_list t))
end

module Map = String.Map
module Infix = Comparator.Operators (T)

let of_local_lib_name s = of_string (Lib_name.Local.to_string s)

let to_local_lib_name s = Lib_name.Local.of_string_exn s

let invalid_module_name ~loc name =
  User_error.raise ~loc [ Pp.textf "invalid module name: %S" name ]

let decode =
  let open Dune_lang.Decoder in
  plain_string (fun ~loc name ->
      if is_valid_module_name name then
        of_string name
      else
        invalid_module_name ~loc name)

module Per_item = Per_item.Make (T)

module Obj = struct
  module T = struct
    type nonrec t = string

    let compare = compare
  end

  include T

  (* We make sure that obj's start with a lowercase letter to make it harder to
     confuse them with a proper module name *)
  let of_name name = String.uncapitalize_ascii name

  let to_name = of_string

  let encode = Dune_lang.Encoder.string

  let of_string s = of_name (of_string s)

  let decode =
    let open Dune_lang.Decoder in
    let+ s = Dune_lang.Decoder.string in
    of_string s

  let of_path path =
    let fn = Path.basename path in
    of_string
      ( match String.index fn '.' with
      | None -> fn
      | Some i -> String.take fn i )

  let to_dyn = to_dyn

  let fname (t : t) ~ext = t ^ ext

  module Map = Map
  module Set = Set
end

let wrap t ~with_ = sprintf "%s__%s" (Obj.of_name with_) t
