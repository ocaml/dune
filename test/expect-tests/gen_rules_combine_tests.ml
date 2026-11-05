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
