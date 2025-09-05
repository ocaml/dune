open Stdune
module Digest = Dune_digest

let%expect_test "directory digest version" =
  (* If this test fails with a new digest value, make sure to update
     [directory_digest_version] in digest.ml.

     The expected value is kept outside of the expect block on purpose so that it
     must be modified manually. *)
  let expected = "b8103f74615da82331f53c68145085fc" in
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
