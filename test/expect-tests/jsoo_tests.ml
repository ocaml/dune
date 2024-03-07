open Stdune
module Jsoo_rules = Dune_rules.Jsoo_rules

let%expect_test _ =
  let test s l =
    match Jsoo_rules.Version.of_string s with
    | None -> print_endline "Could not parse version"
    | Some version ->
      let c = Jsoo_rules.Version.compare version l in
      let r =
        match c with
        | Eq -> "="
        | Lt -> "<"
        | Gt -> ">"
      in
      print_endline r
  in
  (* equal *)
  test "5.0.1" (5, 0);
  [%expect {| = |}];
  test "5.0.0" (5, 0);
  [%expect {| = |}];
  test "5.0" (5, 0);
  [%expect {| = |}];
  test "5" (5, 0);
  [%expect {| = |}];
  test "5.0+1" (5, 0);
  [%expect {| = |}];
  test "5.0~1" (5, 0);
  [%expect {| = |}];
  test "5.0+1" (5, 0);
  [%expect {| = |}];
  test "5.0.1+git-5.0.1-14-g904cf100b0" (5, 0);
  [%expect {| = |}];
  test "5.0.1" (5, 1);
  [%expect {| < |}];
  test "5.0" (5, 1);
  [%expect {| < |}];
  test "5.1.1" (5, 0);
  [%expect {| > |}];
  test "5.1" (5, 0);
  [%expect {| > |}];
  test "4.0.1" (5, 0);
  [%expect {| < |}];
  test "5.0.1" (4, 0);
  [%expect {| > |}];
  ()
;;
