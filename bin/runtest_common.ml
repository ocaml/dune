open Import

module Test_kind = struct
  type t =
    | Runtest of Path.t
    | Cram of Path.t * Source.Cram_test.t
    | Test_executable of
        { dir : Path.t
        ; exe_name : string
        }
    | Inline_tests of
        { dir : Path.t
        ; lib_name : string
        }

  let alias ~contexts = function
    | Cram (dir, cram) ->
      let name = Dune_engine.Alias.Name.of_string (Source.Cram_test.name cram) in
      Alias.in_dir ~name ~recursive:false ~contexts dir
    | Test_executable { dir; exe_name } ->
      (* CR-someday Alizter: get the proper alias, also check js_of_ocaml
         runtst aliases? *)
      let name = Dune_engine.Alias.Name.of_string ("runtest-" ^ exe_name) in
      Alias.in_dir ~name ~recursive:false ~contexts dir
    | Inline_tests { dir; lib_name } ->
      (* CR-someday Alizter: get the proper alias where it is defined. *)
      let name = Dune_engine.Alias.Name.of_string ("runtest-" ^ lib_name) in
      Alias.in_dir ~name ~recursive:false ~contexts dir
    | Runtest dir ->
      Alias.in_dir ~name:Dune_rules.Alias.runtest ~recursive:true ~contexts dir
  ;;
end

(** [classify_ml_test ~sctx ~dir ~ml_file] looks up whether [ml_file] is part
    of a (tests) or (library (inline_tests ...)) stanza in [dir] and returns:

    - [Ok (`Test_executable exe_name)] if the file is a test entry point, or if
      the file belongs to a tests stanza with a single entry point (in which
      case that entry point is returned)

    - [Ok (`Inline_tests_library lib_name)] if the file belongs to a library
      with inline tests enabled

    - [Error `Not_an_entry_point] if the file belongs to a tests stanza with
      multiple entry points but is not itself an entry point

    - [Error `Not_a_test] if the file is not part of any tests stanza or inline
      tests library *)
let classify_ml_test ~sctx ~dir ~ml_file =
  let open Memo.O in
  let ml_file_no_ext = Filename.remove_extension ml_file in
  match Dune_lang.Module_name.of_string_opt ml_file_no_ext with
  | None -> Memo.return (Error `Not_a_test)
  | Some module_name ->
    let* dir_contents =
      Dir_contents.get
        sctx
        ~dir:
          (Path.Build.append_source (Context.build_dir (Super_context.context sctx)) dir)
    in
    let* ml_sources = Dir_contents.ocaml dir_contents
    and* scope = Dir_contents.dir dir_contents |> Dune_rules.Scope.DB.find_by_dir in
    Dune_rules.Ml_sources.find_origin
      ml_sources
      ~libs:(Dune_rules.Scope.libs scope)
      [ module_name ]
    >>| (function
     | Some (Library lib) when Library.has_inline_tests lib ->
       let lib_name = snd lib.name |> Lib_name.Local.to_string in
       Ok (`Inline_tests_library lib_name)
     | Some (Library _ | Executables _ | Melange _) | None -> Error `Not_a_test
     | Some (Tests { exes; _ }) ->
       let exe_names = Nonempty_list.to_list exes.names |> List.map ~f:snd in
       if List.mem exe_names ml_file_no_ext ~equal:String.equal
       then Ok (`Test_executable ml_file_no_ext)
       else (
         match exe_names with
         | [ single_exe ] -> Ok (`Test_executable single_exe)
         | [] | _ :: _ :: _ -> Error `Not_an_entry_point))
;;

(** Looks up a cram test at [path] in [parent_dir]. Returns [Some Test_kind.t]
    if found, [None] otherwise. Raises if the test exists but is missing its
    run.t file. *)
let find_cram ~parent_dir path =
  let open Memo.O in
  Source_tree.find_dir parent_dir
  >>= function
  | None -> Memo.return None
  | Some dir ->
    Dune_rules.Cram_rules.cram_tests dir
    >>| List.find_map ~f:(function
      | Ok cram_test when Path.Source.equal path (Source.Cram_test.path cram_test) ->
        Some (Test_kind.Cram (Path.source parent_dir, cram_test))
      (* Raise any error we encounter when looking for our test specifically. *)
      | Error (Dune_rules.Cram_rules.Missing_run_t cram_test)
        when Path.Source.equal path (Source.Cram_test.path cram_test) ->
        Dune_rules.Cram_rules.missing_run_t cram_test
      (* Any errors or successes unrelated to our test are discarded. *)
      | Error (Dune_rules.Cram_rules.Missing_run_t _) | Ok _ -> None)
;;

(** Looks up an ML test at [path]. Returns [Ok Test_kind.t] if found,
    [Error `Not_an_entry_point] if the file belongs to multiple test
    executables, or [Error `Not_found] if not a test. *)
let find_ml ~sctx ~parent_dir path =
  let open Memo.O in
  let filename = Path.Source.basename path in
  classify_ml_test ~sctx ~dir:parent_dir ~ml_file:filename
  >>| function
  | Ok (`Test_executable exe_name) ->
    Ok (Test_kind.Test_executable { dir = Path.source parent_dir; exe_name })
  | Ok (`Inline_tests_library lib_name) ->
    Ok (Test_kind.Inline_tests { dir = Path.source parent_dir; lib_name })
  | Error `Not_an_entry_point -> Error `Not_an_entry_point
  | Error `Not_a_test -> Error `Not_found
;;

let explain_unsuccessful_search ~sctx path ~parent_dir =
  let open Memo.O in
  let+ candidates =
    Source_tree.find_dir parent_dir
    >>= function
    | None -> Memo.return []
    | Some source_dir ->
      let+ cram_candidates =
        Dune_rules.Cram_rules.cram_tests source_dir
        >>| List.filter_map ~f:(fun res ->
          Result.to_option res
          |> Option.map ~f:(fun test ->
            Source.Cram_test.path test |> Path.Source.to_string))
      and+ ml_test_candidates =
        Source_tree.Dir.filenames source_dir
        |> Filename.Set.to_list
        |> List.filter ~f:(String.is_suffix ~suffix:".ml")
        |> Memo.List.filter ~f:(fun ml_file ->
          classify_ml_test ~sctx ~dir:parent_dir ~ml_file >>| Result.is_ok)
      and+ dir_candidates =
        Source_tree.Dir.sub_dirs source_dir
        |> String.Map.to_list
        |> Memo.List.map ~f:(fun (_candidate, candidate_path) ->
          Source_tree.Dir.sub_dir_as_t candidate_path
          >>| Source_tree.Dir.path
          >>| Path.Source.to_string)
      in
      List.concat [ cram_candidates; ml_test_candidates; dir_candidates ]
      |> String.Set.of_list
      |> String.Set.to_list
  in
  User_error.raise
    ~hints:(User_message.did_you_mean (Path.Source.to_string path) ~candidates)
    [ Pp.textf "%S does not match any known test." (Path.Source.to_string path) ]
;;

(** Classifies [path] as a test by first checking its extension, then looking
    it up via the appropriate finder. Returns a [Test_kind.t] for cram tests,
    test executables, inline test libraries, or directories (for @runtest). *)
let classify_test =
  let open Memo.O in
  let try_directory ~sctx ~parent_dir path =
    Source_tree.find_dir path
    >>= function
    | Some _ -> Memo.return (Test_kind.Runtest (Path.source path))
    | None -> explain_unsuccessful_search ~sctx path ~parent_dir
  in
  fun ~sctx path ->
    match Path.Source.parent path with
    | None -> Memo.return (Test_kind.Runtest (Path.source Path.Source.root))
    | Some parent_dir ->
      (* Rough classification by extension: .t files are likely cram tests,
         implementation files (.ml, .re) are likely ML tests, and anything
         else is assumed to be a directory. The actual validation happens
         in the corresponding find_* functions below. *)
      (match
         let filename = Path.Source.basename path in
         if Source.Cram_test.is_cram_suffix filename
         then `Cram_like
         else (
           match
             Dune_lang.Dialect.DB.find_by_extension
               Dune_lang.Dialect.DB.builtin
               (Filename.extension filename)
           with
           | Some (_, Root.Ocaml.Ml_kind.Impl) -> `Ml_like
           | _ -> `Other)
       with
       | `Cram_like ->
         find_cram ~parent_dir path
         >>= (function
          | Some test -> Memo.return test
          | None -> try_directory ~sctx ~parent_dir path)
       | `Ml_like ->
         find_ml ~sctx ~parent_dir path
         >>= (function
          | Ok test -> Memo.return test
          | Error `Not_an_entry_point ->
            User_error.raise
              [ Pp.textf
                  "%S is used by multiple test executables and cannot be run directly."
                  (Path.Source.basename path)
              ]
          | Error `Not_found -> explain_unsuccessful_search ~sctx path ~parent_dir)
       | `Other -> try_directory ~sctx ~parent_dir path)
;;

let make_request ~scontexts ~to_cwd ~test_paths =
  let contexts =
    Context_name.Map.to_list_map scontexts ~f:(fun _ -> Super_context.context)
  in
  List.map test_paths ~f:(fun dir ->
    let dir = Path.of_string dir |> Path.Expert.try_localize_external in
    let sctx, contexts, src_dir =
      match (Util.check_path contexts dir : Util.checked) with
      | In_build_dir (context, dir) ->
        Context_name.Map.find_exn scontexts (Context.name context), [ context ], dir
      | In_source_dir dir ->
        (* We need to adjust the path here to make up for the current working directory. *)
        let dir =
          Path.Source.L.relative Path.Source.root (to_cwd @ Path.Source.explode dir)
        in
        Context_name.Map.find_exn scontexts Context_name.default, contexts, dir
      | In_private_context _ | In_install_dir _ ->
        User_error.raise
          [ Pp.textf "This path is internal to dune: %s" (Path.to_string_maybe_quoted dir)
          ]
      | External _ ->
        User_error.raise
          [ Pp.textf
              "This path is outside the workspace: %s"
              (Path.to_string_maybe_quoted dir)
          ]
    in
    let open Action_builder.O in
    Action_builder.of_memo (classify_test ~sctx src_dir)
    >>| Test_kind.alias ~contexts
    >>= Alias.request)
  |> Action_builder.all_unit
;;
