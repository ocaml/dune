open Stdune
open Dune_engine
module Generated_rules = Build_config.Gen_rules.Rules

let () = Dune_tests_common.init ()

let run rules =
  Fiber.run (Memo.run rules) ~iter:(fun () ->
    Code_error.raise "Unexpected suspension in generated rules test" [])
;;

let%expect_test "empty generated rule combinations remain deferred" =
  let calls = ref 0 in
  let effectful =
    Generated_rules.create
      (Memo.of_thunk (fun () ->
         incr calls;
         Memo.return Rules.empty))
  in
  List.iter
    [ "both empty", Generated_rules.empty, Generated_rules.empty
    ; "empty left", Generated_rules.empty, effectful
    ; "empty right", effectful, Generated_rules.empty
    ; "same effectful record", effectful, effectful
    ]
    ~f:(fun (name, left, right) ->
      calls := 0;
      let { Generated_rules.rules; _ } = Generated_rules.combine_exn left right in
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
    Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.empty
  in
  let declared =
    Generated_rules.create ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  in
  List.iter
    [ "empty left", Generated_rules.empty, declared
    ; "empty right", declared, Generated_rules.empty
    ]
    ~f:(fun (name, left, right) ->
      let { Generated_rules.build_dir_only_sub_dirs; directory_targets; _ } =
        Generated_rules.combine_exn left right
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
