open Stdune
module Position = Lexbuf.Position
module Compact_position = For_tests.Compact_position

let test (pos : Position.t) =
  match Compact_position.For_tests.small_enough pos with
  | false -> print_endline "position too large"
  | true ->
    let t = Compact_position.of_position pos in
    let pos' = Compact_position.to_position t ~fname:pos.pos_fname in
    if Position.equal pos pos'
    then print_endline "[PASS]"
    else (
      print_endline "[FAIL]";
      printfn "expected:\n%s" (Dyn.to_string (Position.to_dyn_no_file pos));
      printfn "received:\n%s" (Dyn.to_string (Position.to_dyn_no_file pos')))
;;

let%expect_test "round trip tests" =
  let base = Position.none in
  test base;
  [%expect {| [PASS] |}];
  test { base with pos_cnum = 1; pos_lnum = 2; pos_bol = 3 };
  [%expect {| [PASS] |}];
  test { base with pos_cnum = 1_000; pos_lnum = 2_200; pos_bol = 300 };
  [%expect {| [PASS] |}];
  test { base with pos_cnum = 1 lsl 32; pos_lnum = 2_200; pos_bol = 300 };
  [%expect {| position too large |}]
;;
