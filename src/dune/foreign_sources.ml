open Stdune
open Dune_file
module Library = Dune_file.Library

(* TODO: This is a strange module; it seems to add unnecessary indirection for
   accessing foreign sources. It's worth checking if it can be simplified away. *)
type t =
  { libraries : Foreign.Sources.t Lib_name.Map.t
  ; archives : Foreign.Sources.t String.Map.t
  ; executables : Foreign.Sources.t String.Map.t
  }

let for_lib t ~name = Lib_name.Map.find_exn t.libraries name

let for_archive t ~archive_name = String.Map.find_exn t.archives archive_name

let for_exes t ~first_exe = String.Map.find_exn t.executables first_exe

let empty =
  { libraries = Lib_name.Map.empty
  ; archives = String.Map.empty
  ; executables = String.Map.empty
  }

let valid_name language ~loc s =
  match s with
  | ""
  | "."
  | ".." ->
    User_error.raise ~loc
      [ Pp.textf "%S is not a valid %s name." s
          (Foreign.Language.proper_name language)
      ]
  | _ -> s

let eval_foreign_sources (d : _ Dir_with_dune.t) foreign_stubs
    ~(object_map : Foreign.Object_map.t) : Foreign.Sources.t =
  let eval (stubs : Foreign.Stubs.t) =
    let language = stubs.language in
    let osl = stubs.names in
    Ordered_set_lang.Unordered_string.eval_loc osl
      ~key:(fun x -> x)
      ~parse:(fun ~loc s ->
        let s = valid_name language ~loc s in
        let s' = Filename.basename s in
        if s' <> s then
          User_error.raise ~loc
            [ Pp.text
                "Relative part of stub is not necessary and should be \
                 removed. To include sources in subdirectories, use the \
                 (include_subdirs ...) stanza."
            ];
        s')
      ~standard:String.Map.empty
    |> String.Map.map ~f:(fun (loc, name) ->
           match
             let open Option.O in
             let* map = String.Map.find object_map name in
             let+ paths = Foreign.Language.Map.find map language in
             paths
           with
           | Some [ path ] -> (loc, Foreign.Source.make ~stubs ~path)
           | None
           | Some [] ->
             User_error.raise ~loc
               [ Pp.textf "Object %S has no source; %s must be present." name
                   (String.enumerate_one_of
                      ( Foreign.Language.possible_fns language name
                          ~dune_version:d.dune_version
                      |> List.map ~f:(fun s -> "\"" ^ s ^ "\"") ))
               ]
           | Some paths ->
             User_error.raise ~loc
               [ Pp.textf "Multiple %s sources for the same object %S:"
                   (Foreign.Language.proper_name language)
                   name
               ; Pp.enumerate paths ~f:(fun path ->
                     Pp.text
                       (Path.to_string_maybe_quoted
                          (Path.drop_optional_build_context (Path.build path))))
               ; Pp.text "This is not allowed; please rename them."
               ])
  in
  let stub_maps = List.map foreign_stubs ~f:eval in
  List.fold_left stub_maps ~init:String.Map.empty ~f:(fun a b ->
      String.Map.union a b ~f:(fun name (loc, src) (_, another_src) ->
          let path src =
            Path.to_string_maybe_quoted
              (Path.drop_optional_build_context
                 (Path.build (Foreign.Source.path src)))
          in
          User_error.raise ~loc
            [ Pp.textf
                "%S and %S map to the same object name %S. This is not \
                 allowed; please rename them."
                (path another_src) (path src) name
            ]))

let make (d : _ Dir_with_dune.t) ~(object_map : Foreign.Object_map.t) =
  let libs, exes =
    List.filter_partition_map d.data ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib ->
          let all =
            eval_foreign_sources d lib.buildable.foreign_stubs ~object_map
          in
          Left (Left (lib, all))
        | Foreign_library library ->
          let all = eval_foreign_sources d [ library.stubs ] ~object_map in
          Left (Right (library.archive_name, (library.stubs.loc, all)))
        | Executables exes ->
          let all =
            eval_foreign_sources d exes.buildable.foreign_stubs ~object_map
          in
          Right (exes, all)
        | _ -> Skip)
  in
  let libs, foreign_libs = List.partition_map libs ~f:Fn.id in
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
    String.Map.of_list_reducei foreign_libs
      ~f:(fun archive_name (loc1, _) (loc2, _) ->
        User_error.raise ~loc:loc2
          [ Pp.textf
              "Multiple foreign libraries with the same archive name %S; the \
               name has already been taken in %s."
              archive_name
              (Loc.to_file_colon_line loc1)
          ])
    |> String.Map.map ~f:snd
  in
  (* TODO: Make this more type-safe by switching to non-empty lists. *)
  let executables =
    String.Map.of_list_map_exn exes ~f:(fun (exes, m) ->
        (snd (List.hd exes.names), m))
  in
  let () =
    let rev_map =
      List.concat_map libs ~f:(fun (_, c_sources) ->
          String.Map.values c_sources
          |> List.map ~f:(fun (loc, source) ->
                 (Foreign.Source.path source, loc)))
      |> Path.Build.Map.of_list
    in
    match rev_map with
    | Ok _ -> ()
    | Error (_, loc1, loc2) ->
      User_error.raise ~loc:loc2
        [ Pp.text "This c stub is already used in another stanza:"
        ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
        ]
  in
  { libraries; archives; executables }
