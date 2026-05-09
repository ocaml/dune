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

let read_file name =
  let chan = open_in name in
  let size = in_channel_length chan in
  let contents = really_input_string chan size in
  close_in chan;
  contents
;;

let somefile = read_file "somefile"

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
