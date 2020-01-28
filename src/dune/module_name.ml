open Stdune

module T = struct
  type t = string

  let compare = Poly.compare

  let to_dyn = Dyn.Encoder.string
end

include T

let encode = Dune_lang.Encoder.string

let add_suffix = ( ^ )

let of_string = String.capitalize

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
      match name with
      | "" -> invalid_module_name ~loc name
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
          of_string s
        with Exit -> invalid_module_name ~loc name ))

module Per_item = Per_item.Make (T)
