open Stdune

let header_ext = ".h"

module Kind = struct
  type t =
    | C
    | Cxx

  let to_string = function
    | C -> "c"
    | Cxx -> "cpp"

  let pp fmt t : unit = Format.pp_print_string fmt (to_string t)

  type split =
    | Unrecognized
    | Not_allowed_until of Dune_lang.Syntax.Version.t
    | Recognized of string * t

  let cxx_version_introduced ~obj ~dune_version ~version_introduced =
    if dune_version >= version_introduced then
      Recognized (obj, Cxx)
    else
      Not_allowed_until version_introduced

  let split_extension fn ~dune_version =
    match String.rsplit2 fn ~on:'.' with
    | Some (obj, "c") -> Recognized (obj, C)
    | Some (obj, "cpp") -> Recognized (obj, Cxx)
    | Some (obj, "cxx") ->
      cxx_version_introduced ~obj ~dune_version ~version_introduced:(1, 8)
    | Some (obj, "cc") ->
      cxx_version_introduced ~obj ~dune_version ~version_introduced:(1, 10)
    | _ -> Unrecognized

  let possible_exts ~dune_version = function
    | C -> [ ".c" ]
    | Cxx ->
      let exts = [ ".cpp" ] in
      let exts =
        if dune_version >= (1, 10) then
          ".cc" :: exts
        else
          exts
      in
      if dune_version >= (1, 8) then
        ".cxx" :: exts
      else
        exts

  let possible_fns t fn ~dune_version =
    possible_exts t ~dune_version |> List.map ~f:(fun ext -> fn ^ ext)

  module Dict = struct
    type 'a t =
      { c : 'a
      ; cxx : 'a
      }

    let c t = t.c

    let cxx t = t.cxx

    let map { c; cxx } ~f = { c = f c; cxx = f cxx }

    let mapi { c; cxx } ~f = { c = f ~kind:C c; cxx = f ~kind:Cxx cxx }

    let make_both a = { c = a; cxx = a }

    let make ~c ~cxx = { c; cxx }

    let get { c; cxx } = function
      | C -> c
      | Cxx -> cxx

    let add t k v =
      match k with
      | C -> { t with c = v }
      | Cxx -> { t with cxx = v }

    let update t k ~f =
      let v = get t k in
      add t k (f v)

    let merge t1 t2 ~f = { c = f t1.c t2.c; cxx = f t1.cxx t2.cxx }
  end
end

module Source = struct
  type t =
    { kind : Kind.t
    ; path : Path.Build.t
    }

  let kind t = t.kind

  let path t = t.path

  let src_dir t = Path.Build.parent_exn t.path

  let make ~kind ~path = { kind; path }
end

module Sources = struct
  type t = (Loc.t * Source.t) String.Map.t

  let objects (t : t) ~dir ~ext_obj =
    String.Map.keys t
    |> List.map ~f:(fun c -> Path.Build.relative dir (c ^ ext_obj))

  let split_by_kind t =
    let c, cxx =
      String.Map.partition t ~f:(fun (_, s) ->
          match (Source.kind s : Kind.t) with
          | C -> true
          | Cxx -> false)
    in
    { Kind.Dict.c; cxx }
end

let all_possible_exts =
  let exts = Kind.possible_exts ~dune_version:Stanza.latest_version in
  (header_ext :: exts C) @ exts Cxx

let c_cxx_or_header ~fn =
  let ext = Filename.extension fn in
  List.mem ~set:all_possible_exts ext
