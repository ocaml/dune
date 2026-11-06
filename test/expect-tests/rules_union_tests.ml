open Stdune
open Dune_engine

let () = Dune_tests_common.init ()

let%expect_test "alias specifications preserve declaration order across fragments" =
  let dir = Path.Build.relative Path.Build.root "default/aliases" in
  let alias = Alias.make (Alias.Name.of_string "test") ~dir in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let emit names =
    run
      (Rules.collect_unit (fun () ->
         Memo.sequential_iter names ~f:(fun name ->
           let loc = Loc.in_file (Path.source (Path.Source.of_string name)) in
           Rules.Produce.Alias.add_deps alias ~loc (Action_builder.return ()))))
  in
  let first = emit [ "first"; "second" ] in
  let second = emit [ "third"; "fourth" ] in
  let rules = Rules.union second (Rules.union first first) in
  let { Rules.Dir_rules.aliases; _ } =
    Rules.Dir_rules.consume (Rules.find rules (Path.build dir))
  in
  let spec = Alias.Name.Map.find_exn aliases (Alias.name alias) in
  Appendable_list.iter spec ~f:(fun (loc, _) ->
    print_endline (Loc.to_file_colon_line loc));
  [%expect
    {|
    first:1
    second:1
    third:1
    fourth:1
    |}]
;;

let%expect_test "directory rule unions preserve emission identity and order" =
  let dir = Path.Build.relative Path.Build.root "default/union" in
  let emission name =
    let rule =
      Rule.make
        ~targets:(Targets.File.create (Path.Build.relative dir name))
        (Action_builder.return Action.Full.empty)
    in
    rule, Rules.find (Rules.of_rules [ rule ]) (Path.build dir)
  in
  let first, first_rules = emission "first" in
  let second, second_rules = emission "second" in
  let third, third_rules = emission "third" in
  let union = Rules.Dir_rules.union in
  let check name rules expected =
    let { Rules.Dir_rules.rules; aliases } = Rules.Dir_rules.consume rules in
    printfn
      "%s: %b"
      name
      (List.equal ( == ) rules expected && Alias.Name.Map.is_empty aliases)
  in
  let all = union (union third_rules first_rules) second_rules in
  check "creation order" all [ first; second; third ];
  check "empty left" (union Rules.Dir_rules.empty all) [ first; second; third ];
  check "empty right" (union all Rules.Dir_rules.empty) [ first; second; third ];
  check "same map" (union all all) [ first; second; third ];
  check "shared singleton" (union all second_rules) [ first; second; third ];
  check "non-singleton right" (union second_rules all) [ first; second; third ];
  let another_emission = Rules.find (Rules.of_rules [ first ]) (Path.build dir) in
  check
    "distinct emissions of one rule"
    (union first_rules another_emission)
    [ first; first ];
  [%expect
    {|
    creation order: true
    empty left: true
    empty right: true
    same map: true
    shared singleton: true
    non-singleton right: true
    distinct emissions of one rule: true
    |}]
;;
