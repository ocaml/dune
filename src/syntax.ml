open Import

module Version = struct
  module T = struct
    type t =
      | Stable of int * int
      | Unstable

    let compare a b =
      match a, b with
      | Stable _, Unstable -> Lt
      | Unstable, Stable _ -> Gt
      | Unstable, Unstable -> Eq
      | Stable (major_a, minor_a), Stable (major_b, minor_b) ->
        begin
          match Int.compare major_a major_b with
          | (Gt | Lt) as ne -> ne
          | Eq -> Int.compare minor_a minor_b
        end
  end

  include T

  module Infix = Comparable.Operators(T)

  let to_string =
    function
    | Stable (a, b) -> sprintf "%u.%u" a b
    | Unstable -> "unstable"

  let sexp_of_t t = Sexp.unsafe_atom_of_string (to_string t)

  let t : t Sexp.Of_sexp.t =
    let open Sexp.Of_sexp in
    raw >>| function
    | Atom (_loc, A "unstable") ->
      Unstable
    | Atom (loc, A s) -> begin
        try
          Scanf.sscanf s "%u.%u" (fun a b -> Stable (a, b))
        with _ ->
          Loc.fail loc "Atom of the form NNN.NNN expected"
      end
    | sexp ->
      of_sexp_error (Sexp.Ast.loc sexp) "Atom expected"

  let can_read ~parser_version ~data_version =
    match parser_version, data_version with
    | Unstable, Unstable -> true
    | Unstable, Stable _ -> true
    | Stable _, Unstable -> false
    | Stable (parser_major, parser_minor), Stable (data_major, data_minor) ->
    let open Int.Infix in
    parser_major = data_major && parser_minor >= data_minor
end

module Supported_versions = struct
  type t =
    { stable_versions : int Int.Map.t
    ; mutable supports_unstable : bool
    }

  let make l : t =
    match Int.Map.of_list l with
    | Ok stable_versions -> {stable_versions; supports_unstable = false}
    | Error _ ->
      let versions =
        List.map ~f:(fun (major, minor) -> Version.Stable (major, minor)) l
      in
      Exn.code_error
        "Syntax.create"
        [("versions", Sexp.To_sexp.list Version.sexp_of_t versions)]

  let greatest_supported_version t =
    let (major, minor) =
      Option.value_exn (Int.Map.max_binding t.stable_versions)
    in
    Version.Stable (major, minor)

  let is_supported t = function
    | Version.Stable (major, minor) ->
      begin
        match Int.Map.find t.stable_versions major with
        | Some minor' -> minor' >= minor
        | None -> false
      end
    | Unstable -> t.supports_unstable

  let supported_ranges t =
    Int.Map.to_list t.stable_versions |> List.map ~f:(fun (major, minor) ->
      (Version.Stable (major, 0), Version.Stable (major, minor)))
end

type t =
  { name : string
  ; desc : string
  ; key  : Version.t Univ_map.Key.t
  ; supported_versions : Supported_versions.t
  }

let enable_unstable t =
  t.supported_versions.supports_unstable <- true

module Error = struct
  let since loc t ver ~what =
    Loc.fail loc "%s is only available since version %s of %s"
      what (Version.to_string ver) t.desc

  let renamed_in loc t ver ~what ~to_ =
    Loc.fail loc "%s was renamed to '%s' in the %s version of %s"
      what to_ (Version.to_string ver) t.desc

  let deleted_in loc t ?repl ver ~what =
    Loc.fail loc "%s was deleted in version %s of %s%s"
      what (Version.to_string ver) t.desc
      (match repl with
       | None -> ""
       | Some s -> ".\n" ^ s)
end


let create ~name ~desc supported_versions =
  { name
  ; desc
  ; key = Univ_map.Key.create ~name Version.sexp_of_t
  ; supported_versions = Supported_versions.make supported_versions
  }

let name t = t.name

let check_supported t (loc, ver) =
  if not (Supported_versions.is_supported t.supported_versions ver) then
    Loc.fail loc "Version %s of %s is not supported.\n\
                  Supported versions:\n\
                  %s"
      (Version.to_string ver) t.name
      (String.concat ~sep:"\n"
         (List.map (Supported_versions.supported_ranges t.supported_versions)
            ~f:(fun (a, b) ->
              let open Version.Infix in
              if a = b then
                sprintf "- %s" (Version.to_string a)
              else
                sprintf "- %s to %s"
                  (Version.to_string a)
                  (Version.to_string b))))

let greatest_supported_version t =
  Supported_versions.greatest_supported_version t.supported_versions

let key t = t.key

open Sexp.Of_sexp

let set t ver parser =
  set t.key ver parser

let get_exn t =
  get t.key >>| function
  | Some x -> x
  | None ->
    Exn.code_error "Syntax identifier is unset"
      [ "name", Sexp.To_sexp.string t.name ]

let desc () =
  kind >>| fun kind ->
  match kind with
  | Values (loc, None) -> (loc, "This syntax")
  | Fields (loc, None) -> (loc, "This field")
  | Values (loc, Some s) -> (loc, sprintf "'%s'" s)
  | Fields (loc, Some s) -> (loc, sprintf "Field '%s'" s)

let deleted_in t ver =
  let open Version.Infix in
  get_exn t >>= fun current_ver ->
  if current_ver < ver then
    return ()
  else begin
    desc () >>= fun (loc, what) ->
    Error.deleted_in loc t ver ~what
  end

let renamed_in t ver ~to_ =
  let open Version.Infix in
  get_exn t >>= fun current_ver ->
  if current_ver < ver then
    return ()
  else begin
    desc () >>= fun (loc, what) ->
    Error.renamed_in loc t ver ~what ~to_
  end

let since t ver =
  let open Version.Infix in
  get_exn t >>= fun current_ver ->
  if current_ver >= ver then
    return ()
  else begin
    desc () >>= fun (loc, what) ->
    Error.since loc t ver ~what
  end
