open Stdune
open Dune_engine
module Gen_rules = Build_config.Gen_rules

let () = Dune_tests_common.init ()

let generated_rules = function
  | Gen_rules.Rules rules -> rules
  | Unknown_context | Redirect_to_parent _ ->
    Code_error.raise "Expected a normal generated-rules result" []
;;

let run rules =
  Fiber.run (Memo.run rules) ~iter:(fun () ->
    Code_error.raise "Unexpected suspension in generated rules test" [])
;;

let%expect_test "empty generated rule combinations remain deferred" =
  let calls = ref 0 in
  let effectful =
    Gen_rules.make
      (Memo.of_thunk (fun () ->
         incr calls;
         Memo.return Rules.empty))
  in
  List.iter
    [ "both empty", Gen_rules.no_rules, Gen_rules.no_rules
    ; "empty left", Gen_rules.no_rules, effectful
    ; "empty right", effectful, Gen_rules.no_rules
    ; "same effectful record", effectful, effectful
    ]
    ~f:(fun (name, left, right) ->
      calls := 0;
      let { Gen_rules.Rules.rules; _ } =
        Gen_rules.combine left right |> generated_rules
      in
      let deferred = !calls = 0 in
      let result = run rules in
      printfn
        "%s: deferred=%b calls=%d empty=%b"
        name
        deferred
        !calls
        (Path.Build.Map.is_empty (Rules.to_map result)));
  [%expect
    {|
    both empty: deferred=true calls=0 empty=true
    empty left: deferred=true calls=1 empty=true
    empty right: deferred=true calls=1 empty=true
    same effectful record: deferred=true calls=2 empty=true
    |}]
;;

let%expect_test "empty generated rules retain subdirectory declarations" =
  let dir = Path.Build.relative Path.Build.root "default/generated" in
  let build_dir_only_sub_dirs =
    Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.empty
  in
  let declared = Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty) in
  List.iter
    [ "empty left", Gen_rules.no_rules, declared
    ; "empty right", declared, Gen_rules.no_rules
    ]
    ~f:(fun (name, left, right) ->
      let { Gen_rules.Rules.build_dir_only_sub_dirs; directory_targets; _ } =
        Gen_rules.combine left right |> generated_rules
      in
      printfn
        "%s: declaration=%b directory_targets_empty=%b"
        name
        (Path.Build.Map.mem build_dir_only_sub_dirs dir)
        (Path.Build.Map.is_empty directory_targets));
  [%expect
    {|
    empty left: declaration=true directory_targets_empty=true
    empty right: declaration=true directory_targets_empty=true
    |}]
;;

let%expect_test "source subdirectory metadata is combined without evaluating rules" =
  let dir = Path.Build.relative Path.Build.root "default/source" in
  let calls = ref 0 in
  let declared name =
    Gen_rules.Rules
      (Gen_rules.Rules.create
         ~source_sub_dirs:
           (Path.Build.Map.singleton
              dir
              (Filename.Set.singleton (Filename.of_string_exn name)))
         ~source_files:Path.Build.Map.empty
         (Memo.of_thunk (fun () ->
            incr calls;
            Memo.return Rules.empty)))
  in
  let left = declared "left" in
  let right = declared "right" in
  List.iter
    [ "empty left", Gen_rules.no_rules, left
    ; "empty right", left, Gen_rules.no_rules
    ; "both declared", left, right
    ]
    ~f:(fun (name, left, right) ->
      let { Gen_rules.Rules.source_sub_dirs; _ } =
        Gen_rules.combine left right |> generated_rules
      in
      let subdirs =
        Path.Build.Map.find_exn source_sub_dirs dir
        |> Filename.Set.to_list
        |> List.map ~f:Filename.to_string
        |> String.concat ~sep:","
      in
      printfn "%s: subdirs=%s calls=%d" name subdirs !calls);
  [%expect
    {|
    empty left: subdirs=left calls=0
    empty right: subdirs=left calls=0
    both declared: subdirs=left,right calls=0
    |}]
;;

let%expect_test "source file inventories do not force materialization" =
  let dir = Path.Build.relative Path.Build.root "default/source" in
  let calls = ref 0 in
  let declared name =
    let file = Filename.of_string_exn name in
    let materialization =
      Memo.of_thunk (fun () ->
        incr calls;
        Load_rules.source_file_copy
          ~src:(Path.Source.relative_fname Path.Source.root file)
          ~dst:(Path.Build.relative_fname dir file)
        |> Memo.return)
    in
    Gen_rules.Rules
      (Gen_rules.Rules.create
         ~source_sub_dirs:Path.Build.Map.empty
         ~source_files:
           (Path.Build.Map.singleton dir (Filename.Map.singleton file materialization))
         (Memo.return Rules.empty))
  in
  let { Gen_rules.Rules.source_files; _ } =
    Gen_rules.combine (declared "left") (declared "right") |> generated_rules
  in
  let files = Path.Build.Map.find_exn source_files dir in
  printfn
    "files=%s calls=%d"
    (Filename.Map.keys files |> Filename.L.to_string |> String.concat ~sep:",")
    !calls;
  let (_ : Rule.t) = run (Filename.Map.find_exn files (Filename.of_string_exn "left")) in
  printfn "after selecting left: calls=%d" !calls;
  [%expect
    {|
    files=left,right calls=0
    after selecting left: calls=1
    |}]
;;
