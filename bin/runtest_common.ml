open Import

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

let all_tests_of_dir parent_dir =
  let open Memo.O in
  let+ cram_candidates =
    cram_tests_of_dir parent_dir
    >>| List.filter_map ~f:(fun res ->
      Result.to_option res
      |> Option.map ~f:(fun test -> Source.Cram_test.path test |> Path.Source.to_string))
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
  List.concat [ cram_candidates; dir_candidates ]
;;

let explain_unsuccessful_search path ~parent_dir =
  let open Memo.O in
  let+ candidates = all_tests_of_dir parent_dir in
  User_error.raise
    ~hints:(User_message.did_you_mean (Path.Source.to_string path) ~candidates)
    [ Pp.textf "%S does not match any known test." (Path.Source.to_string path) ]
;;

(* [disambiguate_test_name path] is a function that takes in a
   directory [path] and classifies it as either a cram test or a directory to
   run tests in. *)
let disambiguate_test_name path =
  match Path.Source.parent path with
  | None -> Memo.return @@ `Runtest (Path.source Path.Source.root)
  | Some parent_dir ->
    let open Memo.O in
    let* cram_tests = cram_tests_of_dir parent_dir in
    (match find_cram_test cram_tests path with
     | Some test ->
       (* If we find the cram test, then we request that is run. *)
       Memo.return (`Cram (parent_dir, test))
     | None ->
       (* If we don't find it, then we assume the user intended a directory for
          @runtest to be used. *)
       Source_tree.find_dir path
       >>= (function
        (* We need to make sure that this directory or file exists. *)
        | Some _ -> Memo.return (`Runtest (Path.source path))
        | None -> explain_unsuccessful_search path ~parent_dir))
;;

let make_request ~dir_or_cram_test_paths ~to_cwd (setup : Import.Main.build_system) =
  let contexts = setup.contexts in
  List.map dir_or_cram_test_paths ~f:(fun dir ->
    let dir = Path.of_string dir |> Path.Expert.try_localize_external in
    let open Action_builder.O in
    let* contexts, alias_kind =
      match (Util.check_path contexts dir : Util.checked) with
      | In_build_dir (context, dir) ->
        let+ res = Action_builder.of_memo (disambiguate_test_name dir) in
        [ context ], res
      | In_source_dir dir ->
        (* We need to adjust the path here to make up for the current working directory. *)
        let dir =
          Path.Source.L.relative Path.Source.root (to_cwd @ Path.Source.explode dir)
        in
        let+ res = Action_builder.of_memo (disambiguate_test_name dir) in
        contexts, res
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
    Alias.request
    @@
    match alias_kind with
    | `Cram (dir, cram) ->
      let alias_name = Source.Cram_test.name cram in
      Alias.in_dir
        ~name:(Dune_engine.Alias.Name.of_string alias_name)
        ~recursive:false
        ~contexts
        (Path.source dir)
    | `Runtest dir ->
      Alias.in_dir ~name:Dune_rules.Alias.runtest ~recursive:true ~contexts dir)
  |> Action_builder.all_unit
;;
