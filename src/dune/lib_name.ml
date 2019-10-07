open Stdune

let encode = Dune_lang.Encoder.string

let decode = Dune_lang.Decoder.string

module Local = struct
  type t = string

  let valid_char = function
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '_'
    | '0' .. '9' ->
      true
    | _ -> false

  let of_string (name : string) =
    match name with
    | "" -> Error ()
    | (s : string) ->
      if s.[0] = '.' then
        Error ()
      else
        let len = String.length s in
        let rec loop warn i =
          if i = len - 1 then
            if warn then
              Error ()
            else
              Ok s
          else
            let c = String.unsafe_get s i in
            if valid_char c then
              loop warn (i + 1)
            else if c = '.' then
              loop true (i + 1)
            else
              Error ()
        in
        loop false 0

  let of_string_exn s =
    match of_string s with
    | Ok s -> s
    | Error () ->
      Code_error.raise "Lib_name.Local.of_string_exn got invalid name"
        [ ("name", String s) ]

  let decode_loc =
    Dune_lang.Decoder.plain_string (fun ~loc s -> (loc, of_string s))

  let encode = Dune_lang.Encoder.string

  let pp_quoted fmt t = Format.fprintf fmt "%S" t

  let pp fmt t = Format.fprintf fmt "%s" t

  let valid_format_doc =
    Pp.text
      "library names must be non-empty and composed only of the following \
       characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'"

  let validate (loc, res) =
    match res with
    | Ok s -> s
    | Error () ->
      User_error.raise ~loc ~hints:[ valid_format_doc ]
        [ Pp.text "Invalid library name." ]

  let to_string s = s
end

let split t =
  match String.split t ~on:'.' with
  | [] -> assert false
  | pkg :: rest -> (Package.Name.of_string pkg, rest)

let pp = Format.pp_print_string

let pp_quoted fmt t = Format.fprintf fmt "%S" t

let to_local = Local.of_string

let to_dyn t = Dyn.String t

let to_string t = t

let of_string_exn ~loc:_ s = s

let of_local (_loc, t) = t

type t = string

let hash = String.hash

let compare = String.compare

include (
  Comparator.Operators (struct
    type nonrec t = t

    let compare = compare
  end) :
    Comparator.OPS with type t := t )

module O = Comparable.Make (String)
module Map = O.Map

module Set = struct
  include O.Set

  let to_string_list = to_list
end

let root_lib t =
  match String.lsplit2 t ~on:'.' with
  | None -> t
  | Some (p, _) -> p

let package_name t = Package.Name.of_string (root_lib t)

let nest x y = sprintf "%s.%s" x y
