open Stdune
module Digest = Dune_digest

let%expect_test "directory digest version" =
  (* If this test fails with a new digest value, make sure to update
     [directory_digest_version] in digest.ml.

     The expected value is kept outside of the expect block on purpose so that it
     must be modified manually. *)
  let expected = "0707f6c61422e7e7281faa106824da8c" in
  let dir = Temp.create Dir ~prefix:"digest-tests" ~suffix:"" in
  let stats = { Digest.Stats_for_digest.st_kind = S_DIR; executable = true } in
  (match Digest.path_with_stats ~allow_dirs:true dir stats with
   | Ok digest ->
     let digest = Digest.to_string digest in
     if String.equal digest expected
     then print_endline "[PASS]"
     else
       printfn
         "[FAIL] new digest value. please update the version and this test.\n%s"
         digest
   | Error (Unexpected_kind | Unix_error _) ->
     print_endline "[FAIL] unable to calculate digest");
  [%expect {| [PASS] |}]
;;

let%expect_test "directories with symlinks" =
  let dir = Temp.create Dir ~prefix:"digest-tests" ~suffix:"" in
  let stats = { Digest.Stats_for_digest.st_kind = S_DIR; executable = true } in
  let sub = Path.relative dir "sub" in
  Path.mkdir_p sub;
  Unix.symlink "bar" (Path.to_string (Path.relative dir "foo"));
  Unix.symlink "bar" (Path.to_string (Path.relative sub "foo"));
  (match Digest.path_with_stats ~allow_dirs:true dir stats with
   | Ok _ -> print_endline "[PASS]"
   | Error Unexpected_kind -> print_endline "[FAIL] unexpected kind"
   | Error (Unix_error _) -> print_endline "[FAIL] unable to calculate digest");
  [%expect {| [PASS] |}]
;;

let encode_int i =
  let i = Int64.of_int i in
  String.init 8 ~f:(fun byte ->
    Int64.(to_int (logand (shift_right_logical i (8 * byte)) 0xffL)) |> Char.chr)
;;

let%expect_test "manual digest matches concatenated input" =
  let short = String.make 4080 'x' in
  let long = String.make 5000 'y' in
  let nested_digest = Digest.string "nested" in
  let expected_input =
    String.concat
      ~sep:""
      [ encode_int 42
      ; "\001"
      ; encode_int (String.length short)
      ; short
      ; encode_int (String.length long)
      ; long
      ; Digest.to_string_raw nested_digest
      ]
  in
  let manual = Digest.Manual.create () in
  Digest.Manual.int manual 42;
  Digest.Manual.bool manual true;
  Digest.Manual.string manual short;
  Digest.Manual.string manual long;
  Digest.Manual.digest manual nested_digest;
  let actual = Digest.Manual.get manual in
  print_endline (Bool.to_string (Digest.equal actual (Digest.string expected_input)));
  [%expect {| true |}]
;;

let%expect_test "manual digest mixed with repr" =
  let manual = Digest.Manual.create () in
  Digest.Manual.int manual 42;
  Digest.Manual.repr manual (Repr.list Repr.string) [ "foo"; "bar" ];
  Digest.Manual.bool manual false;
  Digest.Manual.string manual "suffix";
  print_endline (Digest.to_string (Digest.Manual.get manual));
  [%expect {| 392a19ccb6e22a69a71d5afe7973cc59 |}]
;;

let%expect_test "repr digest distinguishes option cases" =
  let repr = Option.repr Repr.string in
  let digest_none = Digest.repr repr None in
  let digest_some_empty = Digest.repr repr (Some "") in
  let digest_none' = Digest.repr repr None in
  print_endline (Bool.to_string (Digest.equal digest_none digest_some_empty));
  print_endline (Bool.to_string (Digest.equal digest_none digest_none'));
  [%expect
    {|
    false
    true |}]
;;
