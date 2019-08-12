open Stdune

module T = struct
  type t = string

  let compare = compare
end

include T

let decode = Dune_lang.Decoder.string

let encode = Dune_lang.Encoder.string

let to_dyn = Dyn.Encoder.string

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
