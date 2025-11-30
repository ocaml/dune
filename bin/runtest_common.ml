open Import

module Test_kind = struct
  type t =
    | Runtest of Path.t
    | Cram of Path.t * Source.Cram_test.t
    | Test_executable of
        { dir : Path.t
        ; exe_name : string
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

(** [find_test_executable ~sctx ~dir ~ml_file] looks up whether [ml_file] is part
    of a (tests) stanza in [dir] and returns:
    - [Ok exe_name] if the file is a test entry point, or if the file belongs to
      a tests stanza with a single entry point (in which case that entry point is
      returned)
    - [Error `Not_an_entry_point] if the file belongs to a tests stanza with
      multiple entry points but is not itself an entry point
    - [Error `Not_a_test] if the file is not part of any tests stanza *)
let find_test_executable ~sctx ~dir ~ml_file =
  let open Memo.O in
  let ml_file_no_ext = Filename.remove_extension ml_file in
  match Dune_lang.Module_name.of_string_opt ml_file_no_ext with
  | None -> Memo.return (Error `Not_a_test)
  | Some module_name ->
    let* dir_contents =
      let dir =
        Path.Build.append_source (Super_context.context sctx |> Context.build_dir) dir
      in
      Dir_contents.get sctx ~dir
    in
    let* ml_sources = Dir_contents.ocaml dir_contents
    and* scope = Dir_contents.dir dir_contents |> Dune_rules.Scope.DB.find_by_dir in
    Dune_rules.Ml_sources.find_origin
      ml_sources
      ~libs:(Dune_rules.Scope.libs scope)
      [ module_name ]
    >>| (function
     | Some (Library _ | Executables _ | Melange _) | None -> Error `Not_a_test
     | Some (Tests { exes; _ }) ->
       let exe_names = Nonempty_list.to_list exes.names |> List.map ~f:snd in
       if List.mem exe_names ml_file_no_ext ~equal:String.equal
       then Ok ml_file_no_ext
       else (
         match exe_names with
         | [ single_exe ] -> Ok single_exe
         | _ -> Error `Not_an_entry_point))
;;

let all_tests_of_dir ~sctx parent_dir =
  let open Memo.O in
  let+ cram_candidates =
    cram_tests_of_dir parent_dir
    >>| List.filter_map ~f:(fun res ->
      Result.to_option res
      |> Option.map ~f:(fun test -> Source.Cram_test.path test |> Path.Source.to_string))
  and+ test_executables_candidates =
    let dir =
      Path.Build.append_source
        (Super_context.context sctx |> Context.build_dir)
        parent_dir
    in
    Dir_contents.get sctx ~dir
    >>= Dir_contents.ocaml
    >>| Dune_rules.Ml_sources.test_entry_points
    >>| List.map ~f:(fun name -> name ^ ".ml")
  and+ dir_candidates =
    Source_tree.find_dir parent_dir
    >>= function
    | None -> Memo.return []
    | Some parent_source_dir ->
      Source_tree.Dir.sub_dirs parent_source_dir
      |> String.Map.to_list
      |> Memo.List.map ~f:(fun (_candidate, candidate_path) ->
        Source_tree.Dir.sub_dir_as_t candidate_path
        >>| Source_tree.Dir.path
        >>| Path.Source.to_string)
  in
  List.concat [ cram_candidates; test_executables_candidates; dir_candidates ]
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
  | None -> Memo.return (Test_kind.Runtest (Path.source Path.Source.root))
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
       find_test_executable ~sctx ~dir:parent_dir ~ml_file:filename
       >>= (function
        | Ok exe_name ->
          Memo.return
            (Test_kind.Test_executable { dir = Path.source parent_dir; exe_name })
        | Error `Not_an_entry_point ->
          User_error.raise
            [ Pp.textf
                "%S is used by multiple test executables and cannot be run directly."
                filename
            ]
        | Error `Not_a_test ->
          (* If we don't find it, then we assume the user intended a directory for
             @runtest to be used. *)
          Source_tree.find_dir path
          >>= (function
           (* We need to make sure that this directory or file exists. *)
           | Some _ -> Memo.return (Test_kind.Runtest (Path.source path))
           | None -> explain_unsuccessful_search ~sctx path ~parent_dir)))
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
    Action_builder.of_memo (disambiguate_test_name ~sctx src_dir)
    >>| Test_kind.alias ~contexts
    >>= Alias.request)
  |> Action_builder.all_unit
;;
