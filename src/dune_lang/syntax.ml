open! Stdune

module Version = struct
  module T = struct
    type t = int * int

    let compare (major_a, minor_a) (major_b, minor_b) =
      match Int.compare major_a major_b with
      | (Gt | Lt) as ne -> ne
      | Eq -> Int.compare minor_a minor_b

    let to_dyn t =
      let open Dyn.Encoder in
      pair int int t
  end

  include T
  module Infix = Comparator.Operators (T)

  let equal = Infix.equal

  let to_string (a, b) = sprintf "%u.%u" a b

  let pp fmt t = Format.fprintf fmt "%s" (to_string t)

  let hash = Hashtbl.hash

  let encode t = Encoder.string (to_string t)

  let decode : t Decoder.t =
    let open Decoder in
    raw
    >>| function
    | Atom (loc, A s) -> (
      try Scanf.sscanf s "%u.%u" (fun a b -> (a, b))
      with _ ->
        User_error.raise ~loc [ Pp.text "Atom of the form NNN.NNN expected" ] )
    | sexp -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "Atom expected" ]

  let can_read ~parser_version:(parser_major, parser_minor)
      ~data_version:(data_major, data_minor) =
    let open Int.Infix in
    parser_major = data_major && parser_minor >= data_minor
end

module Supported_versions = struct
  type t = int Int.Map.t

  let to_dyn = Int.Map.to_dyn Int.to_dyn

  let make = Int.Map.of_list_exn

  let greatest_supported_version t = Option.value_exn (Int.Map.max_binding t)

  let is_supported t (major, minor) =
    match Int.Map.find t major with
    | Some minor' -> minor' >= minor
    | None -> false

  let supported_ranges t =
    Int.Map.to_list t
    |> List.map ~f:(fun (major, minor) -> ((major, 0), (major, minor)))
end

type t =
  { name : string
  ; desc : string
  ; key : Version.t Univ_map.Key.t
  ; supported_versions : Supported_versions.t
  }

module Error_msg = struct
  let since t ver ~what =
    Printf.sprintf
      "%s is only available since version %s of %s. Please update your \
       dune-project file to have (lang %s)."
      what (Version.to_string ver) t.desc (Version.to_string ver)
end

module Error = struct
  let since loc t ver ~what =
    User_error.raise ~loc [ Pp.text (Error_msg.since t ver ~what) ]

  let renamed_in loc t ver ~what ~to_ =
    User_error.raise ~loc
      [ Pp.textf "%s was renamed to '%s' in the %s version of %s" what to_
          (Version.to_string ver) t.desc
      ]

  let deleted_in loc t ?(repl = []) ver ~what =
    User_error.raise ~loc
      ( Pp.textf "%s was deleted in version %s of %s" what
          (Version.to_string ver) t.desc
      :: repl )
end

module Warning = struct
  let deprecated_in loc t ?(repl = []) ver ~what =
    User_warning.emit ~loc
      ( Pp.textf "%s was deprecated in version %s of %s." what
          (Version.to_string ver) t.desc
      :: repl )
end

let create ~name ~desc supported_versions =
  { name
  ; desc
  ; key = Univ_map.Key.create ~name Version.to_dyn
  ; supported_versions = Supported_versions.make supported_versions
  }

let name t = t.name

let check_supported t (loc, ver) =
  if not (Supported_versions.is_supported t.supported_versions ver) then
    User_error.raise ~loc
      [ Pp.textf "Version %s of %s is not supported." (Version.to_string ver)
          t.name
      ; Pp.text "Supported versions:"
      ; Pp.enumerate (Supported_versions.supported_ranges t.supported_versions)
          ~f:(fun (a, b) ->
            let open Version.Infix in
            if a = b then
              Pp.text (Version.to_string a)
            else
              Pp.textf "%s to %s" (Version.to_string a) (Version.to_string b))
      ]

let greatest_supported_version t =
  Supported_versions.greatest_supported_version t.supported_versions

let key t = t.key

open Decoder

let set t ver parser = set t.key ver parser

let get_exn t =
  get t.key
  >>= function
  | Some x -> return x
  | None ->
    let+ context = get_all in
    Code_error.raise "Syntax identifier is unset"
      [ ("name", Dyn.Encoder.string t.name)
      ; ("supported_versions", Supported_versions.to_dyn t.supported_versions)
      ; ("context", Univ_map.to_dyn context)
      ]

let desc () =
  let+ kind = kind in
  match kind with
  | Values (loc, None) -> (loc, "This syntax")
  | Fields (loc, None) -> (loc, "This field")
  | Values (loc, Some s) -> (loc, sprintf "'%s'" s)
  | Fields (loc, Some s) -> (loc, sprintf "Field '%s'" s)

let deleted_in t ver =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver < ver then
    return ()
  else
    let* loc, what = desc () in
    Error.deleted_in loc t ver ~what

let deprecated_in t ver =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver < ver then
    return ()
  else
    let+ loc, what = desc () in
    Warning.deprecated_in loc t ver ~what

let renamed_in t ver ~to_ =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver < ver then
    return ()
  else
    let+ loc, what = desc () in
    Error.renamed_in loc t ver ~what ~to_

let since ?(fatal = true) t ver =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver >= ver then
    return ()
  else
    desc ()
    >>= function
    | loc, what when fatal -> Error.since loc t ver ~what
    | loc, what ->
      User_warning.emit ~loc [ Pp.text (Error_msg.since t ver ~what) ];
      return ()
