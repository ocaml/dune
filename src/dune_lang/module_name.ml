open Import

let valid_format_doc = Site.valid_format_doc

include Site.Modulelike (struct
    type t = string

    let description = "module name"
    let module_ = "Module_name"
    let to_string s = s
    let make s = String.capitalize s
  end)

let equal = String.equal
let compare = String.compare
let add_suffix = ( ^ )
let uncapitalize = String.uncapitalize
let pp_quote fmt x = Format.fprintf fmt "%S" x

module Set = struct
  include String.Set

  let to_dyn t = Dyn.Set (List.map ~f:(fun s -> Dyn.String s) (to_list t))
end

module Map = String.Map
module Infix = Comparator.Operators (String)

let of_local_lib_name (loc, s) = parse_string_exn (loc, Lib_name.Local.to_string s)
let to_local_lib_name s = Lib_name.Local.of_string s

module Per_item = struct
  include Per_item.Make (String)
  open Decoder

  let decode ~default a =
    peek_exn
    >>= function
    | List (loc, Atom (_, A "per_module") :: _) ->
      sum
        [ ( "per_module"
          , let+ x =
              repeat
                (let+ pp, names = pair a (repeat decode) in
                 names, pp)
            in
            of_mapping x ~default
            |> function
            | Ok t -> t
            | Error (name, _, _) ->
              User_error.raise
                ~loc
                [ Pp.textf "module %s present in two different sets" (to_string name) ] )
        ]
    | _ -> a >>| for_all
  ;;
end

let of_string_allow_invalid (_loc, s) =
  (* TODO add a warning here that is possible to disable *)
  String.capitalize s
;;

module Unique = struct
  module T = struct
    type nonrec t = string

    let compare = compare
  end

  include T

  let equal x y = Ordering.is_eq (compare x y)

  (* We make sure that obj's start with a lowercase letter to make it harder to
     confuse them with a proper module name *)
  let of_name_assuming_needs_no_mangling name = String.uncapitalize_ascii name

  let to_name t ~loc =
    match of_string_opt t with
    | Some t -> t
    | None ->
      User_error.raise ~loc [ Pp.textf "%s corresponds to an invalid module name" t ]
  ;;

  let encode = Encoder.string
  let of_string s = of_name_assuming_needs_no_mangling (of_string s)
  let to_string s = s

  let decode =
    let open Decoder in
    let+ s = Decoder.string in
    of_string s
  ;;

  let of_path_assuming_needs_no_mangling_allow_invalid path =
    let fn = Path.basename path in
    let loc = Loc.in_file path in
    let name =
      match String.index fn '.' with
      | None -> fn
      | Some i -> String.take fn i
    in
    of_name_assuming_needs_no_mangling (of_string_allow_invalid (loc, name))
  ;;

  let to_dyn = to_dyn
  let artifact_filename (t : t) ~ext = t ^ ext

  module Map = Map
  module Set = Set
end

module Path = struct
  module T = struct
    type nonrec t = t Nonempty_list.t

    let to_dyn t = Dyn.list to_dyn (Nonempty_list.to_list t)
    let compare = Nonempty_list.compare ~compare

    let to_string Nonempty_list.(t :: ts) =
      List.map ~f:to_string (t :: ts) |> String.concat ~sep:"."
    ;;
  end

  include T

  let equal x y = compare x y |> Ordering.is_eq
  let uncapitalize s = to_string s |> String.uncapitalize

  module C = Comparable.Make (T)
  module Set = C.Set
  module Map = C.Map

  let wrap (p :: path : t) =
    Unique.of_name_assuming_needs_no_mangling @@ String.concat ~sep:"__" (p :: path)
  ;;

  let append_double_underscore t = Nonempty_list.(t @ [ "" ])
  let encode (t :: ts : t) = List.map (t :: ts) ~f:encode
  let decode = Decoder.(repeat decode >>| Nonempty_list.of_list_exn)
end

let wrap t ~with_ =
  let (x :: xs : Path.t) = with_ in
  Path.wrap (t :: x :: xs)
;;
