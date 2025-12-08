open Import

module Test_kind = struct
  type t =
    | Runtest of Path.t
    | Cram of Path.t * Source.Cram_test.t
    | Test_executable of Path.t * string (* dir, executable name *)
    | Inline_tests of Path.t * string (* dir, library name *)

  let alias ~contexts = function
    | Cram (dir, cram) ->
      let name = Dune_engine.Alias.Name.of_string (Source.Cram_test.name cram) in
      Alias.in_dir ~name ~recursive:false ~contexts dir
    | Test_executable (dir, exe_name) ->
      (* CR-someday Alizter: get the proper alias, also check js_of_ocaml
         runtst aliases? *)
      let name = Dune_engine.Alias.Name.of_string ("runtest-" ^ exe_name) in
      Alias.in_dir ~name ~recursive:false ~contexts dir
    | Inline_tests (dir, lib_name) ->
      (* CR-someday Alizter: get the proper alias where it is defined. *)
      let name = Dune_engine.Alias.Name.of_string ("runtest-" ^ lib_name) in
      Alias.in_dir ~name ~recursive:false ~contexts dir
    | Runtest dir ->
      Alias.in_dir ~name:Dune_rules.Alias.runtest ~recursive:true ~contexts dir
  ;;
end

let cram_tests_of_dir parent_dir =
  let open Memo.O in
  Source_tree.find_dir parent_dir
  >>= function
  | None -> Memo.return []
  | Some dir -> Dune_rules.Cram_rules.cram_tests dir
;;

let find_cram_test cram_tests path =
  List.find_map cram_tests ~f:(function
    | Ok cram_test when Path.Source.equal path (Source.Cram_test.path cram_test) ->
      Some cram_test
    (* We raise any error we encounter when looking for our test specifically. *)
    | Error (Dune_rules.Cram_rules.Missing_run_t cram_test)
      when Path.Source.equal path (Source.Cram_test.path cram_test) ->
      Dune_rules.Cram_rules.missing_run_t cram_test
      (* Any errors or successes unrelated to our test are discarded. *)
    | Error (Dune_rules.Cram_rules.Missing_run_t _) | Ok _ -> None)
;;

let has_inline_tests (lib : Dune_rules.Library.t) =
  Dune_rules.Sub_system_name.Map.mem
    lib.sub_systems
    Dune_rules.Inline_tests_info.Tests.name
;;

let classify_ml_test ~sctx ~dir ~ml_file =
  let open Memo.O in
  let module_name = Filename.remove_extension ml_file in
  match Dune_lang.Module_name.of_string_opt module_name with
  | None -> Memo.return (Error `Not_a_test)
  | Some module_name ->
    let build_dir =
      Path.Build.append_source (Super_context.context sctx |> Context.build_dir) dir
    in
    let* dir_contents = Dir_contents.get sctx ~dir:build_dir in
    let* ml_sources = Dir_contents.ocaml dir_contents
    and* scope = Dir_contents.dir dir_contents |> Dune_rules.Scope.DB.find_by_dir in
    Dune_rules.Ml_sources.find_origin
      ml_sources
      ~libs:(Dune_rules.Scope.libs scope)
      [ module_name ]
    >>| (function
     | Some (Library lib) when has_inline_tests lib ->
       let lib_name = snd lib.name |> Lib_name.Local.to_string in
       Ok (`Inline_tests_library lib_name)
     | Some (Library _ | Executables _ | Melange _) | None -> Error `Not_a_test
     | Some (Tests ({ exes; _ } as _test)) ->
       let exe_names = Nonempty_list.to_list exes.names |> List.map ~f:snd in
       if List.mem exe_names (Filename.remove_extension ml_file) ~equal:String.equal
       then Ok (`Test_executable (Filename.remove_extension ml_file))
       else (
         match exe_names with
         | [ single_exe ] -> Ok (`Test_executable single_exe)
         | [] | _ :: _ -> Error `Not_an_entry_point))
;;

let all_tests_of_dir ~sctx parent_dir =
  let open Memo.O in
  let+ cram_candidates =
    cram_tests_of_dir parent_dir
    >>| List.filter_map ~f:(fun res ->
      Result.to_option res
      |> Option.map ~f:(fun test -> Source.Cram_test.path test |> Path.Source.to_string))
  and+ test_executable_candidates =
    Source_tree.find_dir parent_dir
    >>= function
    | None -> Memo.return []
    | Some source_dir ->
      Source_tree.Dir.filenames source_dir
      |> Filename.Set.to_list
      |> List.filter ~f:(fun f -> String.is_suffix f ~suffix:".ml")
      |> Memo.List.filter ~f:(fun ml_file ->
        classify_ml_test ~sctx ~dir:parent_dir ~ml_file >>| Result.is_ok)
  and+ dir_candidates =
    let* parent_source_dir = Source_tree.find_dir parent_dir in
    match parent_source_dir with
    | None -> Memo.return []
    | Some parent_source_dir ->
      let dirs = Source_tree.Dir.sub_dirs parent_source_dir in
      String.Map.to_list dirs
      |> Memo.List.map ~f:(fun (_candidate, candidate_path) ->
        Source_tree.Dir.sub_dir_as_t candidate_path
        >>| Source_tree.Dir.path
        >>| Path.Source.to_string)
  in
  List.concat [ cram_candidates; test_executable_candidates; dir_candidates ]
  |> String.Set.of_list
  |> String.Set.to_list
;;

let explain_unsuccessful_search ~sctx path ~parent_dir =
  let open Memo.O in
  let+ candidates = all_tests_of_dir ~sctx parent_dir in
  User_error.raise
    ~hints:(User_message.did_you_mean (Path.Source.to_string path) ~candidates)
    [ Pp.textf "%S does not match any known test." (Path.Source.to_string path) ]
;;

(* [disambiguate_test_name path] is a function that takes in a directory [path]
   and classifies it as either a cram test, test executable, or a directory to
   run tests in. *)
let disambiguate_test_name ~sctx path =
  match Path.Source.parent path with
  | None -> Memo.return @@ Test_kind.Runtest (Path.source Path.Source.root)
  | Some parent_dir ->
    let open Memo.O in
    let* cram_tests = cram_tests_of_dir parent_dir in
    (match find_cram_test cram_tests path with
     | Some test ->
       (* If we find the cram test, then we request that is run. *)
       Memo.return (Test_kind.Cram (Path.source parent_dir, test))
     | None ->
       (* Check for test executables *)
       let filename = Path.Source.basename path in
       let* test_kind_opt =
         classify_ml_test ~sctx ~dir:parent_dir ~ml_file:filename
         >>| function
        | Ok (`Test_executable exe_name) -> Some (`Test_exe exe_name)
        | Ok (`Inline_tests_library lib_name) -> Some (`Inline_tests lib_name)
        | Error `Not_an_entry_point ->
          User_error.raise
            [ Pp.textf
                "%S is used by multiple test executables and cannot be run directly."
                filename
            ]
        | Error `Not_a_test -> None
       in
       (match test_kind_opt with
        | Some (`Test_exe exe_name) ->
          (* Found a test executable for this ML file *)
          Memo.return (Test_kind.Test_executable (Path.source parent_dir, exe_name))
        | Some (`Inline_tests lib_name) ->
          (* Found an inline tests library for this ML file *)
          Memo.return (Test_kind.Inline_tests (Path.source parent_dir, lib_name))
        | None ->
          (* If we don't find it, then we assume the user intended a directory for
             @runtest to be used. *)
          Source_tree.find_dir path
          >>= (function
           (* We need to make sure that this directory or file exists. *)
           | Some _ -> Memo.return (Test_kind.Runtest (Path.source path))
           | None -> explain_unsuccessful_search ~sctx path ~parent_dir)))
;;

let make_request ~contexts ~scontexts ~to_cwd ~test_paths =
  List.map test_paths ~f:(fun dir ->
    let dir = Path.of_string dir |> Path.Expert.try_localize_external in
    let sctx, contexts, src_dir =
      match (Util.check_path contexts dir : Util.checked) with
      | In_build_dir (context, dir) ->
        ( Dune_engine.Context_name.Map.find_exn scontexts (Context.name context)
        , [ context ]
        , dir )
      | In_source_dir dir ->
        (* We need to adjust the path here to make up for the current working directory. *)
        let dir =
          Path.Source.L.relative Path.Source.root (to_cwd @ Path.Source.explode dir)
        in
        ( Dune_engine.Context_name.Map.find_exn scontexts Context_name.default
        , contexts
        , dir )
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
    Action_builder.of_memo (disambiguate_test_name ~sctx src_dir)
    >>| Test_kind.alias ~contexts
    >>= Alias.request)
  |> Action_builder.all_unit
;;
