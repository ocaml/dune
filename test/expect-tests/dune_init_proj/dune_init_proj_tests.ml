let%expect_test "dune init proj" =
  let open Expect_test_helpers in
  within_temp_dir (fun () ->
    run "dune" ["init"; "proj"; "project"];
    print (ls_tree ());
    [%expect {|
      project
    |}];
    within "project" (fun () ->
      print (ls_tree ());
      [%expect {|
        .gitignore
        .ocamlformat
        dune
        dune-project
      |}];
    )
  )
;;