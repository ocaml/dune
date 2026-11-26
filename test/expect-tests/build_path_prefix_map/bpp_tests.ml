open Stdune

let decode_and_print s =
  match Build_path_prefix_map.decode_map s with
  | Error msg -> print_endline ("Error: " ^ msg)
  | Ok map ->
    List.iter map ~f:(function
      | None -> print_endline "(empty)"
      | Some { Build_path_prefix_map.source; target } ->
        Printf.printf "%s=%s\n" target source)
;;

let%expect_test "encode then decode preserves the map" =
  let map =
    [ Some { Build_path_prefix_map.target = "/a%b=c:d"; source = "/with space/é" }
    ; None
    ; Some { target = "$TESTCASE_ROOT"; source = "C:\\Users\\a\\dune\\_build/.sandbox" }
    ]
  in
  let encoded = Build_path_prefix_map.encode_map map in
  print_endline encoded;
  decode_and_print encoded;
  [%expect
    {|
    /a%#b%+c%.d=/with space/é::$TESTCASE_ROOT=C%.\Users\a\dune\_build/.sandbox
    /a%b=c:d=/with space/é
    (empty)
    $TESTCASE_ROOT=C:\Users\a\dune\_build/.sandbox
    |}]
;;

let%expect_test "decoding an unescaped drive-letter colon in a source (#10176)" =
  decode_and_print "/NATIVEPATH=C:/:$TESTCASE_ROOT=C%.\\foo";
  [%expect
    {|
    /NATIVEPATH=C:/
    $TESTCASE_ROOT=C:\foo
    |}]
;;

let%expect_test "decoding a drive-letter colon in a target" =
  decode_and_print "C:\\work\\test=src";
  [%expect
    {|
    C:\work\test=src
    |}]
;;

let%expect_test "a drive-letter target in a non-initial entry stays with its entry" =
  decode_and_print "A=B:C:\\work=/ROOT";
  [%expect
    {|
    A=B
    C:\work=/ROOT
    |}]
;;

let%expect_test "decoding a drive-letter colon in the middle of a map" =
  decode_and_print "a=b:C:/foo:d=e";
  [%expect
    {|
    a=b:C:/foo
    d=e
    |}]
;;

let%expect_test "decode_prefix accepts an unescaped colon" =
  (match Build_path_prefix_map.decode_prefix "C:" with
   | Error msg -> print_endline ("Error: " ^ msg)
   | Ok p -> print_endline p);
  [%expect {| C: |}]
;;

let%expect_test "empty entries are preserved" =
  decode_and_print "A=/a::B=/a/b:";
  [%expect
    {|
    A=/a
    (empty)
    B=/a/b
    (empty)
    |}];
  decode_and_print ":A=B";
  [%expect
    {|
    (empty)
    A=B
    |}]
;;

let%expect_test "empty entries adjacent to rejoined segments" =
  decode_and_print "a=b:C::/x:d=e";
  [%expect
    {|
    a=b:C:/x
    (empty)
    d=e
    |}]
;;

let%expect_test "a colon at the end of a value is ambiguous with an empty entry" =
  decode_and_print "A=B:garbage";
  [%expect {| A=B:garbage |}];
  decode_and_print "A=B:";
  [%expect
    {|
    A=B
    (empty)
    |}]
;;

let%expect_test "malformed maps are errors" =
  decode_and_print "foo:bar";
  [%expect {| Error: invalid key/value pair "foo:bar", no '=' separator |}];
  decode_and_print ":/NOEQUALS";
  [%expect {| Error: invalid key/value pair "/NOEQUALS", no '=' separator |}];
  decode_and_print "a=b=c";
  [%expect {| Error: invalid character '=' in key or value |}]
;;

let%expect_test "rewriting follows right-to-left precedence" =
  let map =
    match Build_path_prefix_map.decode_map "T1=/a:T2=/a/b" with
    | Ok map -> map
    | Error msg -> failwith msg
  in
  print_endline (Build_path_prefix_map.rewrite map "/a/b/c");
  print_endline (Build_path_prefix_map.rewrite map "/a/x");
  [%expect
    {|
    T2/c
    T1/x
    |}]
;;
