let%expect_test "disable typo warnings via environment variable" =
  let dune_project = {|
(lang dune 3.11)
(package
 (name foo)
 (allow_empty)
 (depends
  (bar (= version))
  (baz :with_test)))
|} in
  Unix.putenv "DUNE_CONFIG__TYPO_WARNINGS" "disabled";
  Test_action.write_file "dune-project" dune_project;
  Test_action.run "dune" ["pkg"; "lock"];
  Unix.putenv "DUNE_CONFIG__TYPO_WARNINGS" "enabled"; 
  [%expect {||}]
;;