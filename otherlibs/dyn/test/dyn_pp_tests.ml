let print dyn = print_endline (Dyn.to_string dyn)

(* CR-soon Alizter: Char is currently printed as a raw character, not as an
   OCaml char literal. This means there are no quotes and special characters
   are not escaped. *)

let%expect_test "char - printable" =
  print (Dyn.char 'a');
  [%expect {| a |}];
  print (Dyn.char 'Z');
  [%expect {| Z |}];
  print (Dyn.char '0');
  [%expect {| 0 |}];
  print (Dyn.char ':');
  [%expect {| : |}]
;;

let%expect_test "char - special" =
  print (Dyn.char '\'');
  [%expect {| ' |}];
  print (Dyn.char '"');
  [%expect {| " |}];
  print (Dyn.char '\\');
  [%expect {| \ |}]
;;

(* CR-soon Alizter: Int32/Int64/Nativeint are printed without their literal
   suffix, making them indistinguishable from regular ints. *)

let%expect_test "int32" =
  print (Dyn.int32 42l);
  [%expect {| 42 |}];
  print (Dyn.int32 (-1l));
  [%expect {| -1 |}];
  print (Dyn.int32 0l);
  [%expect {| 0 |}];
  print (Dyn.int32 Int32.max_int);
  [%expect {| 2147483647 |}];
  print (Dyn.int32 Int32.min_int);
  [%expect {| -2147483648 |}]
;;

let%expect_test "int64" =
  print (Dyn.int64 42L);
  [%expect {| 42 |}];
  print (Dyn.int64 (-1L));
  [%expect {| -1 |}];
  print (Dyn.int64 0L);
  [%expect {| 0 |}];
  print (Dyn.int64 Int64.max_int);
  [%expect {| 9223372036854775807 |}];
  print (Dyn.int64 Int64.min_int);
  [%expect {| -9223372036854775808 |}]
;;

let%expect_test "nativeint" =
  print (Dyn.nativeint 42n);
  [%expect {| 42 |}];
  print (Dyn.nativeint (-1n));
  [%expect {| -1 |}];
  print (Dyn.nativeint 0n);
  [%expect {| 0 |}]
;;
