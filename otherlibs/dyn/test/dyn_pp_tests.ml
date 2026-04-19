let print dyn = print_endline (Dyn.to_string dyn)

let%expect_test "char - printable" =
  print (Dyn.char 'a');
  [%expect {| 'a' |}];
  print (Dyn.char 'Z');
  [%expect {| 'Z' |}];
  print (Dyn.char '0');
  [%expect {| '0' |}];
  print (Dyn.char ':');
  [%expect {| ':' |}]
;;

let%expect_test "char - special" =
  print (Dyn.char '\'');
  [%expect {| '\'' |}];
  print (Dyn.char '"');
  [%expect {| '"' |}];
  print (Dyn.char '\\');
  [%expect {| '\\' |}]
;;

let%expect_test "int32" =
  print (Dyn.int32 42l);
  [%expect {| 42l |}];
  print (Dyn.int32 (-1l));
  [%expect {| -1l |}];
  print (Dyn.int32 0l);
  [%expect {| 0l |}];
  print (Dyn.int32 Int32.max_int);
  [%expect {| 2147483647l |}];
  print (Dyn.int32 Int32.min_int);
  [%expect {| -2147483648l |}]
;;

let%expect_test "int64" =
  print (Dyn.int64 42L);
  [%expect {| 42L |}];
  print (Dyn.int64 (-1L));
  [%expect {| -1L |}];
  print (Dyn.int64 0L);
  [%expect {| 0L |}];
  print (Dyn.int64 Int64.max_int);
  [%expect {| 9223372036854775807L |}];
  print (Dyn.int64 Int64.min_int);
  [%expect {| -9223372036854775808L |}]
;;

let%expect_test "nativeint" =
  print (Dyn.nativeint 42n);
  [%expect {| 42n |}];
  print (Dyn.nativeint (-1n));
  [%expect {| -1n |}];
  print (Dyn.nativeint 0n);
  [%expect {| 0n |}]
;;
