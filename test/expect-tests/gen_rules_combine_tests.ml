open Stdune
open Dune_engine
module Gen_rules = Build_config.Gen_rules

let () = Dune_tests_common.init ()

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
      let rules =
        match Gen_rules.combine left right with
        | Rules { rules; _ } -> rules
        | Unknown_context | Redirect_to_parent _ ->
          Code_error.raise "Expected a normal generated-rules result" []
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

let combine left right =
  match Gen_rules.combine left right with
  | Rules rules -> rules
  | Unknown_context | Redirect_to_parent _ ->
    Code_error.raise "Expected a normal generated-rules result" []
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
        combine left right
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
  let calls = ref 0 in
  let declared =
    Gen_rules.Rules
      (Gen_rules.Rules.create
         ~sources:
           (Some
              ( Filename.Map.empty
              , Filename.Array.Set.singleton (Filename.of_string_exn "sub") ))
         (Memo.of_thunk (fun () ->
            incr calls;
            Memo.return Rules.empty)))
  in
  List.iter
    [ "empty left", Gen_rules.no_rules, declared
    ; "empty right", declared, Gen_rules.no_rules
    ]
    ~f:(fun (name, left, right) ->
      let { Gen_rules.Rules.sources; _ } = combine left right in
      let subdirs =
        Option.value_exn sources
        |> snd
        |> Filename.Array.Set.to_list
        |> List.map ~f:Filename.to_string
        |> String.concat ~sep:","
      in
      printfn "%s: subdirs=%s calls=%d" name subdirs !calls);
  [%expect
    {|
    empty left: subdirs=sub calls=0
    empty right: subdirs=sub calls=0
    |}]
;;
