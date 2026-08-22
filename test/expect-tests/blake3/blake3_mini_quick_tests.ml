let digest_of_hex hex =
  match Blake3_mini.Digest.of_hex hex with
  | Some digest -> digest
  | None -> failwith "invalid digest"
;;

let sexp_of_int64 = Sexplib0.Sexp_conv.sexp_of_int64
let quickcheck_generator_int64 = Base_quickcheck.Export.quickcheck_generator_int64
let quickcheck_shrinker_int64 = Base_quickcheck.Export.quickcheck_shrinker_int64

let binary first second =
  let result = Bytes.create 16 in
  Bytes.set_int64_be result 0 first;
  Bytes.set_int64_be result 8 second;
  Bytes.unsafe_to_string result
;;

let digest_of_binary binary = digest_of_hex (Stdlib.Digest.to_hex binary)

let check_equality_and_compare left right =
  let left_digest = digest_of_binary left in
  let right_digest = digest_of_binary right in
  assert (
    Bool.equal
      (Blake3_mini.Digest.equal left_digest right_digest)
      (String.equal left right));
  assert (
    Int.equal
      (Int.compare (Blake3_mini.Digest.compare left_digest right_digest) 0)
      (Int.compare (String.compare left right) 0))
;;

let quick_test_config = { Base_quickcheck.Test.default_config with test_count = 1_000 }

let%quick_test ("digest serialization round trip" [@config quick_test_config]) =
  fun (first : int64) (second : int64) ->
  let binary = binary first second in
  let digest = digest_of_binary binary in
  let same_digest = digest_of_binary binary in
  assert (Blake3_mini.Digest.equal digest same_digest);
  assert (Blake3_mini.Digest.compare digest same_digest = 0);
  assert (Blake3_mini.Digest.hash digest = Blake3_mini.Digest.hash same_digest);
  assert (String.equal (Blake3_mini.Digest.to_binary digest) binary);
  assert (String.equal (Blake3_mini.Digest.to_hex digest) (Stdlib.Digest.to_hex binary))
;;

let%quick_test ("digest equality and comparison" [@config quick_test_config]) =
  fun (left_first : int64)
    (left_second : int64)
    (right_first : int64)
    (right_second : int64) ->
  check_equality_and_compare
    (binary left_first left_second)
    (binary right_first right_second)
;;

let%quick_test ("digest operations with a common prefix" [@config quick_test_config]) =
  fun (first : int64) (left_second : int64) (right_second : int64) ->
  let left = binary first left_second in
  let right = binary first right_second in
  check_equality_and_compare left right;
  assert (
    Int.equal
      (Blake3_mini.Digest.hash (digest_of_binary left))
      (Blake3_mini.Digest.hash (digest_of_binary right)))
;;
