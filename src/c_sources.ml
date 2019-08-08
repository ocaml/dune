open Stdune
open Dune_file

module Library = Dune_file.Library

type t =
  { libraries : C.Sources.t Lib_name.Map.t
  }

let for_lib t ~name = Lib_name.Map.find_exn t.libraries name

let empty =
  { libraries = Lib_name.Map.empty
  }

let c_name, cxx_name =
  let make what ~loc s =
    if match s with
      | "" | "." | ".."  -> true
      | _ -> false then
      User_error.raise ~loc
        [ Pp.textf "%S is not a valid %s name." s what ]
    else
      s
  in
  ( make "C"
  , make "C++"
  )

let load_sources ~dune_version ~dir ~files =
  let init = C.Kind.Dict.make_both String.Map.empty in
  String.Set.fold files ~init ~f:(fun fn acc ->
    match C.Kind.split_extension fn ~dune_version with
    | Unrecognized -> acc
    | Not_allowed_until version ->
      let loc = Loc.in_dir (Path.build dir) in
      User_error.raise ~loc
        [ Pp.textf "Source file %s with extension %s is not allowed \
                    before version %s"
            fn (Filename.extension fn) (Syntax.Version.to_string version)
        ]
    | Recognized (obj, kind) ->
      let path = Path.Build.relative dir fn in
      C.Kind.Dict.update acc kind ~f:(fun v ->
        String.Map.set v obj (C.Source.make ~kind ~path)
      ))

let make (d : _ Dir_with_dune.t)
      ~(c_sources : C.Source.t String.Map.t C.Kind.Dict.t) =
  let libs =
    List.filter_map d.data ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Library lib ->
        let eval (kind : C.Kind.t) (c_sources : C.Source.t String.Map.t)
              validate osl =
          Ordered_set_lang.String.eval_unordered_loc osl
            ~key:(fun x -> x)
            ~parse:(fun ~loc s ->
              let s = validate ~loc s in
              let s' = Filename.basename s in
              if s' <> s then begin
                User_error.raise ~loc
                  [ Pp.text
                      "relative part of stub is not necessary and should be \
                       removed. To include sources in subdirectories, \
                       use the include_subdirs stanza"
                  ]
              end;
              s'
            )
            ~standard:String.Map.empty
          |> String.Map.map ~f:(fun (loc, s) ->
            match String.Map.find c_sources s with
            | Some source -> (loc, source)
            | None ->
              let dune_version = d.dune_version in
              User_error.raise ~loc
                [ Pp.textf "%s does not exist as a C source. %s must \
                            be present"
                    s (String.enumerate_one_of
                         (C.Kind.possible_fns kind s ~dune_version))
                ]
          )
        in
        let names =
          Option.value ~default:Ordered_set_lang.standard in
        let c = eval C.Kind.C c_sources.c c_name (names lib.c_names) in
        let cxx = eval C.Kind.Cxx c_sources.cxx cxx_name (names lib.cxx_names) in
        let all = String.Map.union c cxx ~f:(fun _ (_loc1, c) (loc2, cxx) ->
          User_error.raise ~loc:loc2
            [ Pp.textf "%s and %s have conflicting names. You must \
                        rename one of them."
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context
                      (Path.build (C.Source.path cxx))))
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context
                      (Path.build (C.Source.path c))))
            ]
        ) in
        Some (lib, all)
      | _ -> None
    )
  in
  let libraries =
    match
      Lib_name.Map.of_list_map libs ~f:(fun (lib, m) ->
        Library.best_name lib, m)
    with
    | Ok x -> x
    | Error (name, _, (lib2, _)) ->
      User_error.raise ~loc:lib2.buildable.loc
        [ Pp.textf "Library %S appears for the second time \
                    in this directory"
            (Lib_name.to_string name)
        ]
  in
  let () =
    let rev_map =
      List.concat_map libs ~f:(fun (_, c_sources) ->
        String.Map.values c_sources
        |> List.map ~f:(fun (loc, source) ->
          (C.Source.path source, loc)))
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
  { libraries
  }
