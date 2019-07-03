open Stdune

let encode = Dune_lang.Encoder.string
let decode = Dune_lang.Decoder.string

module Local = struct
  type t = string

  type result =
    | Ok of t
    | Warn of t
    | Invalid

  let valid_char = function
    | 'A'..'Z' | 'a'..'z' | '_' | '0'..'9' -> true
    | _ -> false

  let of_string (name : string) =
    match name with
    | "" -> Invalid
    | (s : string) ->
      if s.[0] = '.' then
        Invalid
      else
        let len = String.length s in
        let rec loop warn i =
          if i = len - 1 then
            if warn then Warn s else Ok s
          else
            let c = String.unsafe_get s i in
            if valid_char c then
              loop warn (i + 1)
            else if c = '.' then
              loop true (i + 1)
            else
              Invalid
        in
        loop false 0

  let of_string_exn s =
    match of_string s with
    | Ok s -> s
    | Warn _
    | Invalid ->
      Code_error.raise "Lib_name.Local.of_string_exn got invalid name"
        [ "name", String s ]

  let decode_loc =
    Dune_lang.Decoder.plain_string (fun ~loc s -> (loc, of_string s))

  let encode = Dune_lang.Encoder.string

  let pp_quoted fmt t = Format.fprintf fmt "%S" t
  let pp fmt t = Format.fprintf fmt "%s" t

  let valid_format_doc =
    Pp.text "library names must be non-empty and composed only of the \
             following characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'"

  let wrapped_warning ~loc ~is_error =
    User_warning.emit ~loc ~hints:[valid_format_doc] ~is_error
      [ Pp.text "Invalid library name."
      ; Pp.text "This is temporary allowed for libraries with (wrapped false)."
      ; Pp.text "It will not be supported in the future. Please choose \
                 a valid name field."
      ]

  let validate (loc, res) ~wrapped =
    match res, wrapped with
    | Ok s, _ -> s
    | Warn _, None
    | Warn _, Some true -> wrapped_warning ~loc ~is_error:true; assert false
    | Warn s, Some false ->
      (* DUNE2: turn this into an error *)
      wrapped_warning ~loc ~is_error:false;
      s
    | Invalid, _ ->
      User_error.raise ~loc ~hints:[valid_format_doc]
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

let compare = String.compare

include (
  Comparator.Operators(struct type nonrec t = t let compare = compare end)
  : Comparator.OPS with type t := t
)

module O = Comparable.Make(String)
module Map = O.Map
module Set = struct
  include O.Set

  let to_string_list = to_list
end

let root_lib t =
  match String.lsplit2 t ~on:'.' with
  | None -> t
  | Some (p, _) -> p

let package_name t =
  Package.Name.of_string (root_lib t)

let nest x y = sprintf "%s.%s" x y
