open Stdune
open Stdune.Csexp
open Dune_tests_common

let () = init ()

let roundtrip x =
  let str = to_string x in
  match parse (Stream.of_string str) with
  | Result.Error e -> failwith e
  | Result.Ok exp ->
    assert (Sexp.compare exp x = Ordering.Eq);
    print (Pp.text str)

let%expect_test _ =
  roundtrip (Sexp.Atom "foo");
  [%expect {|3:foo|}]

let%expect_test _ =
  roundtrip (Sexp.List []);
  [%expect {|()|}]

let%expect_test _ =
  roundtrip (Sexp.List [ Sexp.Atom "Hello"; Sexp.Atom "World!" ]);
  [%expect {|(5:Hello6:World!)|}]

let%expect_test _ =
  roundtrip
    (Sexp.List
       [ Sexp.List
           [ Sexp.Atom "metadata"
           ; Sexp.List [ Sexp.Atom "foo"; Sexp.Atom "bar" ]
           ]
       ; Sexp.List
           [ Sexp.Atom "produced-files"
           ; Sexp.List
               [ Sexp.List
                   [ Sexp.Atom "/tmp/coin"
                   ; Sexp.Atom
                       "/tmp/dune-memory/v2/files/b2/b295e63b0b8e8fae971d9c493be0d261.1"
                   ]
               ]
           ]
       ]);
  [%expect
    {|((8:metadata(3:foo3:bar))(14:produced-files((9:/tmp/coin63:/tmp/dune-memory/v2/files/b2/b295e63b0b8e8fae971d9c493be0d261.1))))|}]

let print_parsed r =
  r
  |> Result.map ~f:Csexp.to_string
  |> Result.to_dyn String.to_dyn String.to_dyn
  |> print_dyn

let parse s = parse (Stream.of_string s) |> print_parsed

let%expect_test _ =
  parse "(3:foo)";
  [%expect {|
    Ok "(3:foo)" |}]

let%expect_test _ =
  parse "";
  [%expect {| Error "unexpected end of file" |}]

let%expect_test _ =
  parse "(";
  [%expect {| Error "unexpected end of file" |}]

let%expect_test _ =
  parse "(a)";
  [%expect {| Error "invalid character in size: a" |}]

let%expect_test _ =
  parse "(:)";
  [%expect {| Error "missing size" |}]

let%expect_test _ =
  parse "(4:foo)";
  [%expect {| Error "unexpected end of file" |}]

let%expect_test _ =
  parse "(5:foo)";
  [%expect {| Error "unexpected end of file in atom of size 5" |}]

let%expect_test _ =
  parse "(3:foo)";
  [%expect {| Ok "(3:foo)" |}]

let%expect_test _ =
  let stream = Stream.of_string "(3:foo)(3:foo)" in
  Csexp.parse stream |> print_parsed;
  [%expect {| Ok "(3:foo)" |}];
  Stream.peek stream |> Option.value_exn |> print_char;
  [%expect {| ( |}]

let%expect_test _ =
  let stream = Stream.of_string "(3:foo)Additional_stuff" in
  Csexp.parse stream |> print_parsed;
  [%expect {| Ok "(3:foo)" |}];
  Stream.peek stream |> Option.value_exn |> print_char;
  [%expect {| A |}]

let%expect_test _ =
  parse_string "(3:foo)(3:foo)" |> print_parsed;
  [%expect {| Error "not whole string consumed" |}]

let%expect_test _ =
  parse_string "(3:foo)additional_stuff" |> print_parsed;
  [%expect {| Error "not whole string consumed" |}]
