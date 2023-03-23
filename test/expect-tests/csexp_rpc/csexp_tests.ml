let%expect_test "printing" =
  let csexp = Csexp.Atom "xxx" in
  print_endline (Csexp.to_string csexp);
  print_int (Csexp.serialised_length csexp);
  [%expect {|
    3:xxx
    5 |}];
  let csexp = Csexp.List [] in
  print_endline (Csexp.to_string csexp);
  print_int (Csexp.serialised_length csexp);
  [%expect {|
        ()
        2 |}];
  let csexp = Csexp.List [ Atom "xxx" ] in
  print_endline (Csexp.to_string csexp);
  print_int (Csexp.serialised_length csexp);
  [%expect {|
            (3:xxx)
            7 |}];
  let csexp = Csexp.List [ Atom "xxx"; Atom "xxx" ] in
  print_endline (Csexp.to_string csexp);
  print_int (Csexp.serialised_length csexp);
  [%expect {|
    (3:xxx3:xxx)
    12 |}]
