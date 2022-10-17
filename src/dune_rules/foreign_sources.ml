open Import
open Dune_file
module Library = Dune_file.Library

(* TODO: This is a strange module; it seems to add unnecessary indirection for
   accessing foreign sources. It's worth checking if it can be simplified away.

   Before this module is removed, there should be a good way to handle new types
   of source files without shoving everything into [Dir_contents].

   Furthemore, this module is also responsible for details such as handling file
   extensions and validating filenames. *)
type t =
  { libraries : Foreign.Sources.t Lib_name.Map.t
  ; archives : Foreign.Sources.t Foreign.Archive.Name.Map.t
  ; executables : Foreign.Sources.t String.Map.t
  }

let for_lib t ~name = Lib_name.Map.find_exn t.libraries name

let for_archive t ~archive_name =
  Foreign.Archive.Name.Map.find_exn t.archives archive_name

let for_exes t ~first_exe = String.Map.find_exn t.executables first_exe

let empty =
  { libraries = Lib_name.Map.empty
  ; archives = Foreign.Archive.Name.Map.empty
  ; executables = String.Map.empty
  }

let valid_name language ~loc s =
  match s with
  | "" | "." | ".." ->
    User_error.raise ~loc
      [ Pp.textf "%S is not a valid %s name." s
          (Foreign_language.proper_name language)
      ]
  | _ -> s

let eval_foreign_stubs foreign_stubs ~dune_version
    ~(sources : Foreign.Sources.Unresolved.t) : Foreign.Sources.t =
  let multiple_sources_error ~name ~loc ~paths =
    User_error.raise ~loc
      [ Pp.textf "Multiple sources map to the same object name %S:" name
      ; Pp.enumerate (List.sort ~compare:Path.Build.compare paths)
          ~f:(fun path ->
            Pp.text
              (Path.to_string_maybe_quoted
                 (Path.drop_optional_build_context (Path.build path))))
      ; Pp.textf
          "This is not allowed; please rename them or remove %S from object \
           names."
          name
      ]
      ~hints:
        [ Pp.text
            "You can also avoid the name clash by placing the objects into \
             different foreign archives and building them in different \
             directories. Foreign archives can be defined using the \
             (foreign_library ...) stanza."
        ]
  in
  let eval (stubs : Foreign.Stubs.t) =
    let language = stubs.language in
    let standard : (Loc.t * string) String.Map.t =
      String.Map.filter_mapi sources ~f:(fun name srcs ->
          List.find_map srcs ~f:(fun (l, _) ->
              Option.some_if
                (Foreign_language.equal l language)
                (stubs.loc, name)))
    in
    let names =
      Ordered_set_lang.Unordered_string.eval_loc stubs.names ~key:Fun.id
        ~standard ~parse:(fun ~loc:_ -> Fun.id)
    in
    String.Map.fold names ~init:String.Map.empty ~f:(fun (loc, s) acc ->
        let name = valid_name language ~loc s in
        let basename = Filename.basename s in
        if name <> basename then
          User_error.raise ~loc
            [ Pp.text
                "Relative part of stub is not necessary and should be removed. \
                 To include sources in subdirectories, use the \
                 (include_subdirs ...) stanza."
            ];
        let open Option.O in
        let source =
          let* candidates = String.Map.find sources name in
          match
            List.filter_map candidates ~f:(fun (l, path) ->
                Option.some_if (Foreign_language.equal l language) path)
          with
          | [ path ] -> Some (loc, Foreign.Source.make ~stubs ~path)
          | [] -> None
          | _ :: _ :: _ as paths -> multiple_sources_error ~name ~loc ~paths
        in
        match source with
        | Some (loc, src) ->
          let new_key = Foreign.Source.object_name src in
          String.Map.add_exn acc new_key (loc, src)
        | None ->
          User_error.raise ~loc
            [ Pp.textf "Object %S has no source; %s must be present." name
                (String.enumerate_one_of
                   (Foreign.possible_sources ~language name ~dune_version
                   |> List.map ~f:(fun s -> sprintf "%S" s)))
            ])
  in
  let stub_maps = List.map foreign_stubs ~f:eval in
  List.fold_left stub_maps ~init:String.Map.empty ~f:(fun a b ->
      String.Map.union a b ~f:(fun _name (loc, src1) (_, src2) ->
          let name = Foreign.Source.object_name src1 in
          multiple_sources_error ~name ~loc
            ~paths:Foreign.Source.[ path src1; path src2 ]))

let check_no_qualified (loc, include_subdirs) =
  if include_subdirs = Dune_file.Include_subdirs.Include Qualified then
    User_error.raise ~loc
      [ Pp.text
          "(include_subdirs qualified) is only meant for OCaml and Coq sources"
      ]

let make stanzas ~(sources : Foreign.Sources.Unresolved.t) ~dune_version
    ~(lib_config : Lib_config.t) =
  let libs, foreign_libs, exes =
    let libs, foreign_libs, exes =
      List.fold_left stanzas ~init:([], [], [])
        ~f:(fun ((libs, foreign_libs, exes) as acc) stanza ->
          match (stanza : Stanza.t) with
          | Library lib ->
            let all =
              eval_foreign_stubs ~dune_version lib.buildable.foreign_stubs
                ~sources
            in
            ((lib, all) :: libs, foreign_libs, exes)
          | Foreign_library library ->
            let all =
              eval_foreign_stubs ~dune_version [ library.stubs ] ~sources
            in
            ( libs
            , (library.archive_name, (library.archive_name_loc, all))
              :: foreign_libs
            , exes )
          | Executables exe | Tests { exes = exe; _ } ->
            let all =
              eval_foreign_stubs ~dune_version exe.buildable.foreign_stubs
                ~sources
            in
            (libs, foreign_libs, (exe, all) :: exes)
          | _ -> acc)
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
             String.Map.to_list_map sources ~f:(fun _ (loc, source) ->
                 (Foreign.Source.object_name source ^ lib_config.ext_obj, loc)))
    in
    match String.Map.of_list objects with
    | Ok _ -> ()
    | Error (path, loc, another_loc) ->
      User_error.raise ~loc
        [ Pp.textf
            "Multiple definitions for the same object file %S. See another \
             definition at %s."
            path
            (Loc.to_file_colon_line another_loc)
        ]
        ~hints:
          [ Pp.text
              "You can avoid the name clash by renaming one of the objects, or \
               by placing it into a different directory."
          ]
  in
  (* TODO: Make this more type-safe by switching to non-empty lists. *)
  let executables =
    String.Map.of_list_map_exn exes ~f:(fun (exes, m) ->
        (snd (List.hd exes.names), m))
  in
  let libraries =
    match
      Lib_name.Map.of_list_map libs ~f:(fun (lib, m) ->
          (Library.best_name lib, m))
    with
    | Ok x -> x
    | Error (name, _, (lib2, _)) ->
      User_error.raise ~loc:lib2.buildable.loc
        [ Pp.textf "Library %S appears for the second time in this directory"
            (Lib_name.to_string name)
        ]
  in
  let archives =
    Foreign.Archive.Name.Map.of_list_reducei foreign_libs
      ~f:(fun archive_name (loc1, _) (loc2, _) ->
        let main_message =
          sprintf "Multiple foreign libraries with the same archive name %S"
            (Foreign.Archive.Name.to_string archive_name)
        in
        let annots =
          let main = User_message.make ~loc:loc2 [ Pp.text main_message ] in
          let related =
            [ User_message.make ~loc:loc1 [ Pp.text "Name already used here" ] ]
          in
          User_message.Annots.singleton Compound_user_error.annot
            (Compound_user_error.make ~main ~related)
        in
        User_error.raise ~annots ~loc:loc2
          [ Pp.textf "%s; the name has already been taken in %s." main_message
              (Loc.to_file_colon_line loc1)
          ])
    |> Foreign.Archive.Name.Map.map ~f:snd
  in
  { libraries; archives; executables }

let make stanzas ~dune_version ~include_subdirs ~(lib_config : Lib_config.t)
    ~dirs =
  check_no_qualified include_subdirs;
  let init = String.Map.empty in
  let sources =
    List.fold_left dirs ~init ~f:(fun acc (dir, _local, files) ->
        let sources =
          Foreign.Sources.Unresolved.load ~dir ~dune_version ~files
        in
        String.Map.Multi.rev_union sources acc)
  in
  make stanzas ~dune_version ~sources ~lib_config
