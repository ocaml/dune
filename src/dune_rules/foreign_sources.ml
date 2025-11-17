open Import
open Memo.O

type t =
  { libraries : Foreign.Sources.t Lib_name.Map.t
  ; archives : Foreign.Sources.t Foreign.Archive.Name.Map.t
  ; executables : Foreign.Sources.t String.Map.t
  }

let for_lib t ~name = Lib_name.Map.find_exn t.libraries name

let for_archive t ~archive_name =
  Foreign.Archive.Name.Map.find_exn t.archives archive_name
;;

let for_exes t ~first_exe = String.Map.find_exn t.executables first_exe

let empty =
  { libraries = Lib_name.Map.empty
  ; archives = Foreign.Archive.Name.Map.empty
  ; executables = String.Map.empty
  }
;;

let multiple_sources_error ~name ~(mode : Mode.Select.t) ~loc ~paths =
  let hints =
    [ Pp.text
        "You can also avoid the name clash by placing the objects into different foreign \
         archives and building them in different directories. Foreign archives can be \
         defined using the (foreign_library ...) stanza."
    ]
  in
  let hints, for_mode =
    match mode with
    | All -> hints, ""
    | Only m ->
      let mode_hint =
        Pp.text
          "You may be missing a mode field that would restrict this stub to some \
           specific mode."
      in
      mode_hint :: hints, Printf.sprintf " for mode %s" @@ Mode.to_string m
  in
  User_error.raise
    ~loc
    [ Pp.textf "Multiple sources map to the same object name %S%s:" name for_mode
    ; Pp.enumerate (List.sort ~compare:Path.Build.compare paths) ~f:(fun path ->
        Pp.text
          (Path.to_string_maybe_quoted
             (Path.drop_optional_build_context (Path.build path))))
    ; Pp.textf
        "This is not allowed; please rename them or remove %S from object names."
        name
    ]
    ~hints
;;

module Unresolved = struct
  type t = (Foreign_language.t * Path.Build.t) String.Map.Multi.t

  let _to_dyn t =
    let entry_to_dyn (language, path) =
      Dyn.Tuple [ Foreign_language.to_dyn language; Path.Build.to_dyn path ]
    in
    String.Map.to_dyn (Dyn.list entry_to_dyn) t
  ;;

  let drop_source_extension fn ~dune_version =
    let open Option.O in
    let* obj, ext = String.rsplit2 fn ~on:'.' in
    let* language, version = String.Map.find Foreign_language.source_extensions ext in
    Option.some_if (dune_version >= version) (obj, language)
  ;;

  let load ~dune_version ~dir ~files =
    let init = String.Map.empty in
    String.Set.fold files ~init ~f:(fun fn acc ->
      match drop_source_extension fn ~dune_version with
      | None -> acc
      | Some (obj, language) ->
        let path = Path.Build.relative dir fn in
        String.Map.add_multi acc obj (language, path))
  ;;

  let find_source sources language (loc, name) =
    let open Option.O in
    let* candidates = String.Map.find sources name in
    match
      List.filter_map candidates ~f:(fun (l, path) ->
        Option.some_if (Foreign_language.equal l language) path)
    with
    | [ path ] -> Some path
    | [] -> None
    | _ :: _ :: _ as paths -> multiple_sources_error ~mode:All ~name ~loc ~paths
  ;;

  let load_dirs ~dune_version dirs =
    List.fold_left
      dirs
      ~init:String.Map.empty
      ~f:(fun acc { Source_file_dir.dir; path_to_root = _; files; source_dir = _ } ->
        let sources = load ~dir ~dune_version ~files in
        String.Map.Multi.rev_union sources acc)
  ;;
end

let possible_sources ~language obj ~dune_version =
  String.Map.to_list Foreign_language.source_extensions
  |> List.filter_map ~f:(fun (ext, (lang, version)) ->
    Option.some_if
      (Foreign_language.equal lang language && dune_version >= version)
      (obj ^ "." ^ ext))
;;

let valid_name language ~loc s =
  match s with
  | "" | "." | ".." ->
    User_error.raise
      ~loc
      [ Pp.textf "%S is not a valid %s name." s (Foreign_language.proper_name language) ]
  | _ -> s
;;

let ctypes_stubs sources (ctypes : Ctypes_field.t) =
  String.Map.of_list_map_exn
    ctypes.function_description
    ~f:(fun (fd : Ctypes_field.Function_description.t) ->
      let loc =
        Loc.none
        (* TODO *)
      in
      let name =
        Ctypes_field.c_generated_functions_cout_c ctypes fd |> Filename.remove_extension
      in
      let path =
        match Unresolved.find_source sources C (loc, name) with
        | Some p -> p
        | None ->
          (* impossible b/c ctypes fields generates this *)
          assert false
      in
      let source = Foreign.Source.make (Ctypes ctypes) ~path in
      name, (loc, source))
;;

let eval_foreign_stubs
      foreign_stubs
      (ctypes : Ctypes_field.t option)
      ~dune_version
      ~(sources : Unresolved.t)
  : Foreign.Sources.t
  =
  let eval (stubs : Foreign.Stubs.t) =
    let language = stubs.language in
    let names =
      let standard : (Loc.t * string) String.Map.t =
        String.Map.filter_mapi sources ~f:(fun name srcs ->
          List.find_map srcs ~f:(fun (l, _) ->
            Option.some_if (Foreign_language.equal l language) (stubs.loc, name)))
      in
      Ordered_set_lang.Unordered_string.eval_loc
        stubs.names
        ~key:Fun.id
        ~standard
        ~parse:(fun ~loc:_ -> Fun.id)
    in
    String.Map.fold names ~init:String.Map.empty ~f:(fun (loc, s) acc ->
      let name = valid_name language ~loc s in
      let basename = Filename.basename s in
      if name <> basename
      then
        User_error.raise
          ~loc
          [ Pp.text
              "Relative part of stub is not necessary and should be removed. To include \
               sources in subdirectories, use the (include_subdirs ...) stanza."
          ];
      match Unresolved.find_source sources language (loc, name) with
      | Some path ->
        let src = Foreign.Source.make (Stubs stubs) ~path in
        let new_key = Foreign.Source.object_name src in
        String.Map.add_exn acc new_key (loc, src)
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf
              "Object %S has no source; %s must be present."
              name
              (String.enumerate_one_of
                 (possible_sources ~language name ~dune_version
                  |> List.map ~f:(fun s -> sprintf "%S" s)))
          ])
  in
  let stub_maps =
    let init = List.map foreign_stubs ~f:eval in
    match ctypes with
    | None -> init
    | Some ctypes -> ctypes_stubs sources ctypes :: init
  in
  List.fold_left stub_maps ~init:String.Map.empty ~f:(fun a b ->
    String.Map.union a b ~f:(fun _name (loc, src1) (_, src2) ->
      let name = Foreign.Source.user_object_name src1 in
      let mode = Foreign.Source.mode src1 in
      multiple_sources_error
        ~name
        ~loc
        ~mode
        ~paths:Foreign.Source.[ path src1; path src2 ]))
  |> Foreign.Sources.make
;;

let make stanzas ~(sources : Unresolved.t) ~dune_version =
  let libs, foreign_libs, exes =
    let eval_foreign_stubs = eval_foreign_stubs ~dune_version ~sources in
    let libs, foreign_libs, exes =
      List.fold_left
        stanzas
        ~init:([], [], [])
        ~f:(fun (libs, foreign_libs, exes) stanza ->
          match stanza with
          | `Library (lib : Library.t) ->
            let all =
              eval_foreign_stubs lib.buildable.foreign_stubs lib.buildable.ctypes
            in
            (lib, all) :: libs, foreign_libs, exes
          | `Foreign_library (library : Foreign_library.t) ->
            let all = eval_foreign_stubs [ library.stubs ] None in
            ( libs
            , (library.archive_name, (library.archive_name_loc, all)) :: foreign_libs
            , exes )
          | `Executables exe | `Tests { Tests.exes = exe; _ } ->
            let all =
              eval_foreign_stubs exe.buildable.foreign_stubs exe.buildable.ctypes
            in
            libs, foreign_libs, (exe, all) :: exes)
    in
    List.(rev libs, rev foreign_libs, rev exes)
  in
  let () =
    let objects =
      List.concat
        [ List.map libs ~f:snd
        ; List.map foreign_libs ~f:(fun (_, (_, sources)) -> sources)
        ; List.map exes ~f:snd
        ]
      |> List.concat_map ~f:(fun sources ->
        Foreign.Sources.to_list_map sources ~f:(fun _ (loc, source) ->
          Foreign.Source.object_name source, loc))
    in
    match String.Map.of_list objects with
    | Ok _ -> ()
    | Error (path, loc, another_loc) ->
      let main_message =
        sprintf "Multiple definitions for the same object file %S" path
      in
      let annots =
        let main = User_message.make ~loc [ Pp.text main_message ] in
        let related =
          [ User_message.make ~loc:another_loc [ Pp.text "Object already defined here" ] ]
        in
        User_message.Annots.singleton
          Compound_user_error.annot
          [ Compound_user_error.make ~main ~related ]
      in
      User_error.raise
        ~loc
        ~annots
        [ Pp.textf
            "%s. See another definition at %s."
            main_message
            (Loc.to_file_colon_line another_loc)
        ]
        ~hints:
          [ Pp.text
              "You can avoid the name clash by renaming one of the objects, or by \
               placing it into a different directory."
          ]
  in
  (* TODO: Make this more type-safe by switching to non-empty lists. *)
  let executables =
    match
      String.Map.of_list_map exes ~f:(fun (exes, m) ->
        let first_exe = snd (Nonempty_list.hd exes.names) in
        first_exe, m)
    with
    | Ok m -> m
    | Error (exe_name, (exes1, _), _) ->
      let loc = fst (Nonempty_list.hd exes1.names) in
      User_error.raise
        ~loc
        [ Pp.textf "Executables with same name %S use different foreign sources" exe_name
        ]
  in
  let libraries =
    match Lib_name.Map.of_list_map libs ~f:(fun (lib, m) -> Library.best_name lib, m) with
    | Ok x -> x
    | Error (name, _, (lib2, _)) ->
      User_error.raise
        ~loc:lib2.buildable.loc
        [ Pp.textf
            "Library %S appears for the second time in this directory"
            (Lib_name.to_string name)
        ]
  in
  let archives =
    Foreign.Archive.Name.Map.of_list_reducei
      foreign_libs
      ~f:(fun archive_name (loc1, _) (loc2, _) ->
        let main_message =
          sprintf
            "Multiple foreign libraries with the same archive name %S"
            (Foreign.Archive.Name.to_string archive_name)
        in
        let annots =
          let main = User_message.make ~loc:loc2 [ Pp.text main_message ] in
          let related =
            [ User_message.make ~loc:loc1 [ Pp.text "Name already used here" ] ]
          in
          User_message.Annots.singleton
            Compound_user_error.annot
            [ Compound_user_error.make ~main ~related ]
        in
        User_error.raise
          ~annots
          ~loc:loc2
          [ Pp.textf
              "%s; the name has already been taken in %s."
              main_message
              (Loc.to_file_colon_line loc1)
          ])
    |> Foreign.Archive.Name.Map.map ~f:snd
  in
  { libraries; archives; executables }
;;

let make stanzas ~dune_version ~dirs =
  let sources = Unresolved.load_dirs ~dune_version dirs in
  make stanzas ~dune_version ~sources
;;

let make stanzas ~dir ~dune_version ~dirs =
  let+ stanzas =
    List.filter_map stanzas ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Library.T lib -> Some (`Library lib, lib.enabled_if)
      | Foreign_library.T lib -> Some (`Foreign_library lib, lib.enabled_if)
      | Executables.T exe -> Some (`Executables exe, exe.enabled_if)
      | Tests.T ({ exes = exe; _ } as tests) -> Some (`Tests tests, exe.enabled_if)
      | _ -> None)
    |> Memo.parallel_map ~f:(fun (stanza, enabled_if) ->
      let* expander = Expander0.get ~dir in
      Expander0.eval_blang expander enabled_if
      >>| function
      | false -> None
      | true -> Some stanza)
    >>| List.filter_opt
  in
  make stanzas ~dune_version ~dirs
;;
