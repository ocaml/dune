let printf = Printf.printf

let test_string =
  let in_ = open_in "somefile" in
  let res = input_line in_ in
  close_in in_;
  res
;;

let%expect_test "128 bits fd" =
  let fd = Unix.openfile "somefile" [ O_RDONLY ] 0 in
  let hash = Blake3_mini.fd fd in
  Unix.close fd;
  printf "%s\n" (Blake3_mini.Digest.to_hex hash);
  [%expect {| 7a7d692dfca02a756fea9a8a77903807 |}]
;;

let%expect_test "file with size" =
  let hash, size = Blake3_mini.file_with_size "somefile" in
  printf "%s %d\n" (Blake3_mini.Digest.to_hex hash) size;
  [%expect {| 7a7d692dfca02a756fea9a8a77903807 11 |}]
;;

let read_file name =
  let chan = open_in name in
  let size = in_channel_length chan in
  let contents = really_input_string chan size in
  close_in chan;
  contents
;;

let somefile = read_file "somefile"

let digest_of_hex hex =
  match Blake3_mini.Digest.of_hex hex with
  | Some digest -> digest
  | None -> failwith "invalid digest"
;;

let%expect_test "digest representation" =
  let digest = digest_of_hex "000102030405060708090a0b0c0d0e0f" in
  let repr = Obj.repr digest in
  printf "payload bytes: %d\n" (Obj.size repr * (Sys.word_size / 8));
  printf "unboxed fields: %b\n" (Obj.tag repr = Obj.double_array_tag);
  [%expect
    {|
    payload bytes: 16
    unboxed fields: true
    |}]
;;

let%expect_test "digest operations" =
  let digest = digest_of_hex "000102030405060708090a0b0c0d0e0f" in
  let same = digest_of_hex "000102030405060708090a0b0c0d0e0f" in
  let different_second = digest_of_hex "000102030405060708090a0b0c0d0e10" in
  let different_first = digest_of_hex "010102030405060708090a0b0c0d0e0f" in
  let nan_bits = digest_of_hex "7ff800000000000108090a0b0c0d0e0f" in
  let same_nan_bits = digest_of_hex "7ff800000000000108090a0b0c0d0e0f" in
  let different_nan_bits = digest_of_hex "7ff800000000000208090a0b0c0d0e0f" in
  printf
    "equal: %b %b %b\n"
    (Blake3_mini.Digest.equal digest same)
    (Blake3_mini.Digest.equal digest different_second)
    (Blake3_mini.Digest.equal digest different_first);
  printf
    "nan bits equal: %b %b\n"
    (Blake3_mini.Digest.equal nan_bits same_nan_bits)
    (Blake3_mini.Digest.equal nan_bits different_nan_bits);
  let compare_is_antisymmetric x y =
    let xy = Blake3_mini.Digest.compare x y in
    let yx = Blake3_mini.Digest.compare y x in
    xy <> 0 && Int.compare xy 0 = -Int.compare yx 0
  in
  printf
    "compare: %b %b %b %b\n"
    (Blake3_mini.Digest.compare digest same = 0)
    (compare_is_antisymmetric digest different_second)
    (compare_is_antisymmetric digest different_first)
    (compare_is_antisymmetric nan_bits different_nan_bits);
  printf
    "lexicographic compare: %b\n"
    (Blake3_mini.Digest.compare
       (digest_of_hex "7fffffffffffffff08090a0b0c0d0e0f")
       (digest_of_hex "800000000000000008090a0b0c0d0e0f")
     < 0);
  printf
    "same-prefix hashes: %b\n"
    (Blake3_mini.Digest.hash digest = Blake3_mini.Digest.hash different_second);
  [%expect
    {|
    equal: true false false
    nan bits equal: true false
    compare: true true true true
    lexicographic compare: true
    same-prefix hashes: true
    |}]
;;

let%expect_test "digest with hasher" =
  let hasher = Blake3_mini.create () in
  Blake3_mini.feed_string hasher somefile ~pos:0 ~len:(String.length somefile);
  let digest = Blake3_mini.digest hasher in
  printf "%s\n" (Blake3_mini.Digest.to_hex digest);
  [%expect {| 7a7d692dfca02a756fea9a8a77903807 |}]
;;

let%expect_test "digest in pieces" =
  let hasher = Blake3_mini.create () in
  let slice_len = 10 in
  assert (slice_len <= String.length somefile);
  Blake3_mini.feed_string hasher somefile ~pos:0 ~len:slice_len;
  Blake3_mini.feed_string
    hasher
    somefile
    ~pos:slice_len
    ~len:(String.length somefile - slice_len);
  let digest = Blake3_mini.digest hasher in
  printf "%s\n" (Blake3_mini.Digest.to_hex digest);
  [%expect {| 7a7d692dfca02a756fea9a8a77903807 |}]
;;

let%expect_test "digest with hasher bigstring" =
  let hasher = Blake3_mini.create () in
  let somefile =
    Bigarray.Array1.init Char C_layout (String.length somefile) (String.get somefile)
  in
  Blake3_mini.feed_bigstring_release_lock
    hasher
    somefile
    ~pos:0
    ~len:(Bigarray.Array1.size_in_bytes somefile);
  let digest = Blake3_mini.digest hasher in
  printf "%s\n" (Blake3_mini.Digest.to_hex digest);
  [%expect {| 7a7d692dfca02a756fea9a8a77903807 |}]
;;

let report_invalid_range name f =
  match f () with
  | () -> printf "%s: accepted\n" name
  | exception Invalid_argument _ -> printf "%s: rejected\n" name
;;

let%expect_test "invalid feed ranges" =
  let hasher = Blake3_mini.create () in
  report_invalid_range "string" (fun () ->
    Blake3_mini.feed_string hasher "x" ~pos:1 ~len:1);
  report_invalid_range "bytes" (fun () ->
    Blake3_mini.feed_bytes hasher (Bytes.of_string "x") ~pos:1 ~len:1);
  let bigstring = Bigarray.Array1.create Char C_layout 1 in
  report_invalid_range "bigstring" (fun () ->
    Blake3_mini.feed_bigstring_release_lock hasher bigstring ~pos:2 ~len:0);
  [%expect
    {|
    string: rejected
    bytes: rejected
    bigstring: rejected
    |}]
;;
