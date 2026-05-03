open Import
include Dune_section

let compare : t -> t -> Ordering.t = Poly.compare
let equal : t -> t -> bool = Poly.equal

let repr =
  Repr.variant
    "section"
    (List.map Dune_section.all ~f:(fun (section, name) ->
       Repr.case0 (String.uppercase_ascii name) ~test:(equal section)))
;;

let to_dyn = Repr.to_dyn repr

module Key = struct
  type nonrec t = t

  let compare = compare
  let to_dyn = to_dyn
end

module O = Comparable.Make (Key)
module Map = O.Map
module Set = O.Set

let parse_string s =
  match of_string s with
  | Some s -> Ok s
  | None -> Error (sprintf "invalid section: %s" s)
;;

let enum_decoder = Dune_section.all |> List.map ~f:(fun (x, y) -> y, x)
let decode = Decoder.enum enum_decoder

let encode v =
  let open Dune_sexp.Encoder in
  string (to_string v)
;;

let all = Set.of_list (List.map ~f:fst Dune_section.all)

let should_set_executable_bit = function
  | Lib | Lib_root | Toplevel | Share | Share_root | Etc | Doc | Man | Misc -> false
  | Libexec | Libexec_root | Bin | Sbin | Stublibs -> true
;;
