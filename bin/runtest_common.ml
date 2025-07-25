open! Import

let find_cram_test path ~parent_dir =
  let open Memo.O in
  Source_tree.nearest_dir parent_dir
  >>= Dune_rules.Cram_rules.cram_tests
  (* We ignore the errors we get when searching for cram tests as they will
     be reported during building anyway. We are only interested in the
     presence of cram tests. *)
  >>| List.filter_map ~f:Result.to_option
  (* We search our list of known cram tests for the test we are looking
     for. *)
  >>| List.find ~f:(fun (test : Source.Cram_test.t) ->
    let src =
      match test with
      | File src -> src
      | Dir { dir = src; _ } -> src
    in
    Path.Source.equal path src)
;;

let explain_unsuccessful_search path ~parent_dir =
  let open Memo.O in
  (* If the user misspelled the test name, we give them a hint. *)
  let+ hints =
    (* We search for all files and directories in the parent directory and
       suggest them as possible candidates. *)
    let+ candidates =
      let+ file_candidates =
        let+ files = Source_tree.files_of parent_dir in
        Path.Source.Set.to_list_map files ~f:Path.Source.to_string
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
      List.concat [ file_candidates; dir_candidates ]
    in
    User_message.did_you_mean (Path.Source.to_string path) ~candidates
  in
  User_error.raise
    ~hints
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
    find_cram_test path ~parent_dir
    >>= (function
     | Some test ->
       (* If we find the cram test, then we request that is run. *)
       Memo.return (`Test (parent_dir, Source.Cram_test.name test))
     | None ->
       (* If we don't find it, then we assume the user intended a directory for
          @runtest to be used. *)
       Source_tree.find_dir path
       >>= (function
        (* We need to make sure that this directory or file exists. *)
        | Some _ -> Memo.return (`Runtest (Path.source path))
        | None -> explain_unsuccessful_search path ~parent_dir))
;;

let make_request ~dir_or_cram_test_paths (setup : Import.Main.build_system) =
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
    | `Test (dir, alias_name) ->
      Alias.in_dir
        ~name:(Dune_engine.Alias.Name.of_string alias_name)
        ~recursive:false
        ~contexts
        (Path.source dir)
    | `Runtest dir ->
      Alias.in_dir ~name:Dune_rules.Alias.runtest ~recursive:true ~contexts dir)
  |> Action_builder.all_unit
;;
