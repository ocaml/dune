open! Import
include Dune_section

let compare : t -> t -> Ordering.t = Poly.compare

let to_dyn x =
  let s = Dune_section.to_string x in
  let open Dyn in
  variant (String.uppercase_ascii s) []

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

let enum_decoder = Dune_section.all |> List.map ~f:(fun (x, y) -> (y, x))

let decode = Dune_lang.Decoder.enum enum_decoder

let encode v =
  let open Dune_lang.Encoder in
  string (to_string v)

let all = Set.of_list (List.map ~f:fst Dune_section.all)

let should_set_executable_bit = function
  | Lib | Lib_root | Toplevel | Share | Share_root | Etc | Doc | Man | Misc ->
    false
  | Libexec | Libexec_root | Bin | Sbin | Stublibs -> true

let valid_format_doc =
  Pp.text
    "Module names must be non-empty and composed only of the following \
     characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'."

module Modulelike (S : sig
  type t

  val module_ : string

  val description : string

  val to_string : t -> string

  val make : string -> t
end) =
Stringlike.Make (struct
  include S

  let valid_char = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '\'' | '_' -> true
    | _ -> false

  let description_of_valid_string = Some valid_format_doc

  let is_valid_first_char = function
    | 'A' .. 'Z' | 'a' .. 'z' -> true
    | _ -> false

  let has_valid_first_char s = is_valid_first_char s.[0]

  let hint_valid =
    Some
      (fun name ->
        let name =
          String.filter_map name ~f:(fun c ->
              if valid_char c then Some c
              else
                match c with
                | '.' | '-' -> Some '_'
                | _ -> None)
        in
        if has_valid_first_char name then name else "M" ^ name)

  let is_valid_module_name name =
    name <> "" && has_valid_first_char name && String.for_all name ~f:valid_char

  let of_string_opt s = if is_valid_module_name s then Some (S.make s) else None
end)

module Site = struct
  include String

  include (
    Modulelike (struct
      type t = string

      let to_string t = t

      let module_ = "Section.Site"

      let description = "site name"

      let make s = s
    end) :
      Stringlike_intf.S with type t := t)

  module Infix = Comparator.Operators (String)
end

let dune_site_syntax =
  Dune_lang.Syntax.create ~name:"dune_site" ~experimental:true
    ~desc:"the sites locations extension (experimental)"
    [ ((0, 1), `Since (2, 8)) ]
