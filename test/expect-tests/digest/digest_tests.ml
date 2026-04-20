open Stdune
module Digest = Dune_digest

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
