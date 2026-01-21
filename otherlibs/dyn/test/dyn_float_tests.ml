let print_float f = print_endline (Dyn.to_string (Dyn.float f))

let%expect_test "basic values" =
  print_float 1.0;
  [%expect {| 1. |}];
  print_float 1.5;
  [%expect {| 1.5 |}];
  print_float 3.14;
  [%expect {| 3.14 |}];
  print_float 0.0;
  [%expect {| 0. |}];
  print_float (-123.0);
  [%expect {| -123. |}]
;;

let%expect_test "special values" =
  print_float nan;
  [%expect {| nan |}];
  print_float infinity;
  [%expect {| infinity |}];
  print_float neg_infinity;
  [%expect {| neg_infinity |}]
;;

let%expect_test "scientific notation" =
  print_float 1e20;
  [%expect {| 1e+20 |}];
  print_float 1e-20;
  [%expect {| 1e-20 |}]
;;

let%expect_test "precision (15g sufficient)" =
  print_float 3.14;
  [%expect {| 3.14 |}];
  print_float 2.718281828459045;
  [%expect {| 2.7182818284590451 |}];
  print_float 1.4142135623730951;
  [%expect {| 1.4142135623730951 |}];
  print_float 0.1;
  [%expect {| 0.1 |}];
  print_float 0.2;
  [%expect {| 0.2 |}]
;;

let%expect_test "precision (17g required)" =
  (* These values are exactly representable but need 17 digits to round-trip *)
  print_float 0.30000000000000004;
  [%expect {| 0.30000000000000004 |}];
  (* 1 + epsilon: smallest float > 1.0, needs 17 digits to distinguish from 1.0 *)
  print_float 1.0000000000000002;
  [%expect {| 1.0000000000000002 |}]
;;

let%expect_test "representation limits" =
  (* 0.1 + 0.2 is famously not exactly 0.3 due to binary representation *)
  print_float (0.1 +. 0.2);
  [%expect {| 0.30000000000000004 |}];
  (* Extra precision beyond what float can represent, rounds to 1.0 *)
  print_float 1.00000000000000002;
  [%expect {| 1. |}];
  (* 2^53 + 1: beyond integer precision of 64-bit floats, rounds to 2^53 *)
  print_float 9007199254740993.;
  [%expect {| 9007199254740992. |}]
;;

let%expect_test "edge cases" =
  (* Smallest positive float *)
  print_float Float.min_float;
  [%expect {| 2.2250738585072014e-308 |}];
  (* Largest finite float *)
  print_float Float.max_float;
  [%expect {| 1.7976931348623157e+308 |}];
  (* Negative zero *)
  print_float (-0.0);
  [%expect {| -0. |}];
  (* Subnormal *)
  print_float 5e-324;
  [%expect {| 4.94065645841247e-324 |}]
;;

let%expect_test "round-trip verification" =
  (* Verify that parsing the output gives back the original value *)
  let check x =
    let s = Dyn.to_string (Dyn.float x) in
    (* float_of_string doesn't accept "neg_infinity", handle specially *)
    let y = if s = "neg_infinity" then neg_infinity else float_of_string s in
    if x = y || (x <> x && y <> y) (* nan <> nan *)
    then print_endline "ok"
    else Printf.printf "FAIL: %s -> %f\n" s y
  in
  check 0.0;
  [%expect {| ok |}];
  check 1.0;
  [%expect {| ok |}];
  check (-1.0);
  [%expect {| ok |}];
  check 3.14159265358979323846;
  [%expect {| ok |}];
  check 0.30000000000000004;
  [%expect {| ok |}];
  check 1e100;
  [%expect {| ok |}];
  check 1e-100;
  [%expect {| ok |}];
  check nan;
  [%expect {| ok |}];
  check infinity;
  [%expect {| ok |}];
  check neg_infinity;
  [%expect {| ok |}]
;;
