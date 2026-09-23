open Stdune
open Dune_engine

let () = Dune_tests_common.init ()

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
