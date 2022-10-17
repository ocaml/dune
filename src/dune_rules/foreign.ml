open Import

let encode_lang = function
  | Foreign_language.C -> "c"
  | Cxx -> "cxx"

let decode_lang =
  let open Foreign_language in
  Dune_lang.Decoder.enum [ (encode_lang C, C); (encode_lang Cxx, Cxx) ]

let drop_source_extension fn ~dune_version =
  let open Option.O in
  let* obj, ext = String.rsplit2 fn ~on:'.' in
  let* language, version =
    String.Map.find Foreign_language.source_extensions ext
  in
  Option.some_if (dune_version >= version) (obj, language)

let possible_sources ~language obj ~dune_version =
  List.filter_map (String.Map.to_list Foreign_language.source_extensions)
    ~f:(fun (ext, (lang, version)) ->
      Option.some_if
        (Foreign_language.equal lang language && dune_version >= version)
        (obj ^ "." ^ ext))

let add_mode_suffix mode s =
  match mode with
  | Mode.Select.All -> s
  | Only mode -> String.concat ~sep:"_" [ s; Mode.to_string mode ]

module Archive = struct
  module Name = struct
    include String

    let to_string t = t

    let path ~dir ~mode archive_name =
      let archive_name = add_mode_suffix mode archive_name in
      Path.Build.relative dir archive_name

    let decode =
      Dune_lang.Decoder.plain_string (fun ~loc s ->
          match s with
          | "." | ".." ->
            User_error.raise ~loc
              [ Pp.textf "%S is not a valid archive name." s ]
          | fn when String.exists fn ~f:Path.is_dir_sep ->
            User_error.raise ~loc
              [ Pp.textf "Path separators are not allowed in archive names." ]
          | fn -> fn)

    let stubs archive_name = archive_name ^ "_stubs"

    let lib_file_prefix = "lib"

    let lib_file archive_name ~dir ~ext_lib ~mode =
      let archive_name = add_mode_suffix mode archive_name in
      Path.Build.relative dir
        (sprintf "%s%s%s" lib_file_prefix archive_name ext_lib)

    let dll_file archive_name ~dir ~ext_dll ~mode =
      let archive_name = add_mode_suffix mode archive_name in
      Path.Build.relative dir (sprintf "dll%s%s" archive_name ext_dll)
  end

  (** Archive directories can appear as part of the [(foreign_archives ...)]
      fields. For example, in [(foreign_archives some/dir/lib1 lib2)], the
      archive [some/dir/lib1] has the directory [some/dir], whereas the archive
      [lib2] does not specify the directory and is assumed to be located in [.]. *)
  module Dir = struct
    type t = string
  end

  type t =
    { dir : Dir.t
    ; name : Name.t
    }

  let dir_path ~dir t = Path.Build.relative dir t.dir

  let name ~mode t = add_mode_suffix mode t.name

  let stubs archive_name = { dir = "."; name = Name.stubs archive_name }

  let decode =
    let open Dune_lang.Decoder in
    let+ s = string in
    { dir = Filename.dirname s; name = Filename.basename s }

  let lib_file ~archive ~dir ~ext_lib ~mode =
    let dir = dir_path ~dir archive in
    Name.lib_file archive.name ~dir ~ext_lib ~mode

  let dll_file ~archive ~dir ~ext_dll ~mode =
    let dir = dir_path ~dir archive in
    Name.dll_file archive.name ~dir ~ext_dll ~mode
end

module Stubs = struct
  module Include_dir_without_include = struct
    type t =
      | Dir of String_with_vars.t
      | Lib of Loc.t * Lib_name.t

    let decode : t Dune_lang.Decoder.t =
      let open Dune_lang.Decoder in
      let parse_dir =
        let+ s = String_with_vars.decode in
        Dir s
      in
      let parse_lib =
        sum
          [ ( "lib"
            , let+ loc, lib_name = located Lib_name.decode in
              Lib (loc, lib_name) )
          ]
      in
      parse_dir <|> parse_lib
  end

  module Include_dir = struct
    include
      Recursive_include.Make
        (Include_dir_without_include)
        (struct
          let include_keyword = "include"

          let include_allowed_in_versions = `Since (3, 5)

          let non_sexp_behaviour = `Parse_as_base_term
        end)

    module Without_include = Include_dir_without_include
  end

  type t =
    { loc : Loc.t
    ; language : Foreign_language.t
    ; names : Ordered_set_lang.t
    ; mode : Mode.Select.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; include_dirs : Include_dir.t list
    ; extra_deps : Dep_conf.t list
    }

  let make ~loc ~language ~names ~mode ~flags =
    { loc; language; names; mode; flags; include_dirs = []; extra_deps = [] }

  let syntax =
    let name = "mode_specific_stubs" in
    let desc = "syntax extension for mode-specific foreign stubs" in
    Dune_lang.Syntax.create ~name ~desc [ ((0, 1), `Since (3, 5)) ]

  let () =
    Dune_project.Extension.register_simple syntax (Dune_lang.Decoder.return [])

  let decode_stubs ~for_library =
    let open Dune_lang.Decoder in
    let* loc = loc in
    let+ loc_archive_name, archive_name =
      located (field_o "archive_name" string)
    and+ language = field "language" decode_lang
    and+ names = Ordered_set_lang.field "names"
    and+ loc_mode, mode =
      located
        (field_o "mode" (Dune_lang.Syntax.since syntax (0, 1) >>> Mode.decode))
    and+ flags = Ordered_set_lang.Unexpanded.field "flags"
    and+ include_dirs =
      field ~default:[] "include_dirs" (repeat Include_dir.decode)
    and+ extra_deps = field_o "extra_deps" (repeat Dep_conf.decode) in
    let extra_deps = Option.value ~default:[] extra_deps in
    let () =
      match archive_name with
      | None -> ()
      | Some _ ->
        User_error.raise ~loc:loc_archive_name
          [ Pp.textf
              "The field \"archive_name\" is not allowed in the (foreign_stubs \
               ...) stanza. For named foreign archives use the \
               (foreign_library ...) stanza."
          ]
    in
    let () =
      match mode with
      | Some _ when for_library ->
        User_error.raise ~loc:loc_mode
          [ Pp.textf "The field \"mode\" is not available for foreign libraries"
          ]
      | _ -> ()
    in
    let mode = Mode.Select.of_option mode in
    { loc; language; names; mode; flags; include_dirs; extra_deps }

  let decode = Dune_lang.Decoder.fields @@ decode_stubs ~for_library:false

  let is_mode_dependent t = Mode.Select.is_not_all t.mode
end

module Objects = struct
  module Object_name = struct
    let decode = Dune_lang.Decoder.string

    let filename t ~ext_obj = t ^ ext_obj

    let build_path t ~error_loc ~ext_obj ~dir =
      Path.Build.relative ~error_loc dir (filename t ~ext_obj)
  end

  (* Associate each object name with its location in the config *)
  type t = (Loc.t * string) list

  let empty = []

  let is_empty = List.is_empty

  let decode =
    let open Dune_lang.Decoder in
    let+ t = repeat (located Object_name.decode) in
    (* Check for duplicate names *)
    match String.Map.of_list (List.map t ~f:Tuple.T2.swap) with
    | Ok _ -> t
    | Error (name, loc, loc') ->
      User_error.raise ~loc
        [ Pp.textf "Duplicate object name: %s. Already appears at:" name
        ; Pp.textf "- %s" (Loc.to_file_colon_line loc')
        ]

  let build_paths t ~ext_obj ~dir =
    (* Foreign objects are not mode-dependent *)
    List.map t ~f:(fun (loc, name) ->
        Object_name.build_path name ~error_loc:loc ~ext_obj ~dir |> Path.build)
end

module Library = struct
  type t =
    { archive_name : Archive.Name.t
    ; archive_name_loc : Loc.t
    ; stubs : Stubs.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ archive_name_loc, archive_name =
         located (field "archive_name" Archive.Name.decode)
       and+ stubs = Stubs.decode_stubs ~for_library:true in
       { archive_name; archive_name_loc; stubs })
end

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

  let object_name t =
    t.path |> Path.Build.split_extension |> fst |> Path.Build.basename
    |> add_mode_suffix t.stubs.mode

  let make ~stubs ~path = { stubs; path }
end

module Sources = struct
  type t = (Loc.t * Source.t) String.Map.t

  let object_files t ~dir ~ext_obj =
    String.Map.keys t
    |> List.map ~f:(fun c -> Path.Build.relative dir (c ^ ext_obj))

  let has_cxx_sources (t : t) =
    String.Map.exists t ~f:(fun (_loc, source) ->
        Foreign_language.(equal Cxx source.stubs.language))

  module Unresolved = struct
    type t = (Foreign_language.t * Path.Build.t) String.Map.Multi.t

    let to_dyn t =
      String.Map.to_dyn
        (fun xs ->
          Dyn.List
            (List.map xs ~f:(fun (language, path) ->
                 Dyn.Tuple
                   [ Foreign_language.to_dyn language; Path.Build.to_dyn path ])))
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
