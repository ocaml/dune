open Stdune

module Language = struct
  module T = struct
    type t =
      | C
      | Cxx

    let compare x y =
      match (x, y) with
      | C, C -> Eq
      | C, _ -> Lt
      | _, C -> Gt
      | Cxx, Cxx -> Eq

    let equal x y =
      match (x, y) with
      | C, C -> true
      | Cxx, Cxx -> true
      | _, _ -> false

    let to_dyn = function
      | C -> Dyn.Variant ("C", [])
      | Cxx -> Dyn.Variant ("Cxx", [])
  end

  include T

  let proper_name = function
    | C -> "C"
    | Cxx -> "C++"

  let encode = function
    | C -> "c"
    | Cxx -> "cxx"

  let decode = Dune_lang.Decoder.enum [ (encode C, C); (encode Cxx, Cxx) ]

  include Comparable.Make (T)

  module Dict = struct
    type 'a t =
      { c : 'a
      ; cxx : 'a
      }

    let c t = t.c

    let cxx t = t.cxx

    let map { c; cxx } ~f = { c = f c; cxx = f cxx }

    let mapi { c; cxx } ~f = { c = f ~language:C c; cxx = f ~language:Cxx cxx }

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

let header_extension = ".h"

let source_extentions =
  String.Map.of_list_exn
    [ ("c", (Language.C, (1, 0)))
    ; ("cpp", (Cxx, (1, 0)))
    ; ("cxx", (Cxx, (1, 8)))
    ; ("cc", (Cxx, (1, 10)))
    ]

let has_foreign_extension ~fn =
  let ext = Filename.extension fn in
  ext = header_extension
  || String.Map.mem source_extentions (String.drop ext 1)

let drop_source_extension fn ~dune_version =
  let open Option.O in
  let* obj, ext = String.rsplit2 fn ~on:'.' in
  let* language, version = String.Map.find source_extentions ext in
  Option.some_if (dune_version >= version) (obj, language)

let possible_sources ~language fn ~dune_version =
  List.filter_map (String.Map.to_list source_extentions)
    ~f:(fun (ext, (lang, version)) ->
      Option.some_if
        (lang == language && dune_version >= version)
        (fn ^ "." ^ ext))

module Stubs = struct
  type t =
    { loc : Loc.t
    ; language : Language.t
    ; names : Ordered_set_lang.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; include_dirs : String_with_vars.t list
    ; extra_deps : Dep_conf.t list
    }

  let make ~loc ~language ~names ~flags =
    { loc; language; names; flags; include_dirs = []; extra_deps = [] }

  let decode_stubs =
    let open Dune_lang.Decoder in
    let+ loc = loc
    and+ loc_archive_name, archive_name =
      located (field_o "archive_name" string)
    and+ language = field "language" Language.decode
    and+ names = field "names" Ordered_set_lang.decode
    and+ flags = Ordered_set_lang.Unexpanded.field "flags"
    and+ include_dirs =
      field ~default:[] "include_dirs" (repeat String_with_vars.decode)
    and+ extra_deps = field_o "extra_deps" (repeat Dep_conf.decode) in
    let extra_deps = Option.value ~default:[] extra_deps in
    let () =
      match archive_name with
      | None -> ()
      | Some _ ->
        User_error.raise ~loc:loc_archive_name
          [ Pp.textf
              "The field \"archive_name\" is not allowed in the \
               (foreign_stubs ...) stanza. For named foreign archives use the \
               (foreign_library ...) stanza."
          ]
    in
    { loc; language; names; flags; include_dirs; extra_deps }

  let decode = Dune_lang.Decoder.fields decode_stubs
end

module Library = struct
  type t =
    { archive_name : string
    ; archive_name_loc : Loc.t
    ; stubs : Stubs.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ archive_name_loc, archive_name =
         located (field "archive_name" string)
       and+ stubs = Stubs.decode_stubs in
       { archive_name; archive_name_loc; stubs })
end

let lib_file ~archive_name ~dir ~ext_lib =
  Path.Build.relative dir (sprintf "lib%s%s" archive_name ext_lib)

let dll_file ~archive_name ~dir ~ext_dll =
  Path.Build.relative dir (sprintf "dll%s%s" archive_name ext_dll)

module Source = struct
  (* we store the entire [stubs] record even though [t] only describes an
     individual source file *)
  type t =
    { stubs : Stubs.t
    ; path : Path.Build.t
    }

  let language t = t.stubs.language

  let flags t = t.stubs.flags

  let path t = t.path

  let make ~stubs ~path = { stubs; path }
end

module Sources = struct
  type t = (Loc.t * Source.t) String.Map.t

  let object_files t ~dir ~ext_obj =
    String.Map.keys t
    |> List.map ~f:(fun c -> Path.Build.relative dir (c ^ ext_obj))

  module Unresolved = struct
    type t = (Language.t * Path.Build.t) String.Map.Multi.t

    let to_dyn t =
      String.Map.to_dyn
        (fun xs ->
          Dyn.List
            (List.map xs ~f:(fun (language, path) ->
                 Dyn.Tuple [ Language.to_dyn language; Path.Build.to_dyn path ])))
        t

    let load ~dune_version ~dir ~files =
      let init = String.Map.empty in
      String.Set.fold files ~init ~f:(fun fn acc ->
          match drop_source_extension fn ~dune_version with
          | None -> acc
          | Some (obj, language) ->
            let path = Path.Build.relative dir fn in
            String.Map.add_multi acc obj (language, path))
  end
end
