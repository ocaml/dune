open Stdune
include Dune_section

let compare : t -> t -> Ordering.t = Poly.compare

let to_dyn x =
  let s = Dune_section.to_string x in
  let open Dyn in
  variant (String.uppercase_ascii s) []
;;

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
let decode = Dune_sexp.Decoder.enum enum_decoder

let encode v =
  let open Dune_sexp.Encoder in
  string (to_string v)
;;

let all = Set.of_list (List.map ~f:fst Dune_section.all)

let should_set_executable_bit = function
  | Lib | Lib_root | Toplevel | Share | Share_root | Etc | Doc | Man | Misc -> false
  | Libexec | Libexec_root | Bin | Sbin | Stublibs -> true
;;
