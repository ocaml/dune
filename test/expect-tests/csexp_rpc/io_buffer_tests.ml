open Stdune
module Io_buffer = Csexp_rpc.Private.Io_buffer

let () = Printexc.record_backtrace false
let print_dyn x = Io_buffer.to_dyn x |> Dyn.to_string |> print_endline

let%expect_test "empty buffer is empty" =
  print_dyn (Io_buffer.create ~size:4);
  [%expect {| { total_written = 0; contents = ""; pos_w = 0; pos_r = 0 } |}]
;;

let%expect_test "resize" =
  let buf = Io_buffer.create ~size:2 in
  Io_buffer.write_csexps buf [ Csexp.Atom "xxx" ];
  print_dyn buf;
  [%expect {|
    { total_written = 0; contents = "3:xxx"; pos_w = 5; pos_r = 0 } |}];
  Io_buffer.write_csexps buf [ Csexp.Atom "xxxyyy" ];
  print_dyn buf;
  [%expect
    {|
    { total_written = 0; contents = "3:xxx6:xxxyyy"; pos_w = 13; pos_r = 0 } |}]
;;

let%expect_test "reading" =
  let buf = Io_buffer.create ~size:10 in
  Io_buffer.write_csexps buf [ Csexp.Atom "abcde" ];
  print_dyn buf;
  [%expect {|
    { total_written = 0; contents = "5:abcde"; pos_w = 7; pos_r = 0 } |}];
  Io_buffer.read buf 4;
  print_dyn buf;
  [%expect {|
    { total_written = 4; contents = "cde"; pos_w = 7; pos_r = 4 } |}];
  Io_buffer.read buf 2;
  print_dyn buf;
  [%expect {|
    { total_written = 6; contents = "e"; pos_w = 7; pos_r = 6 } |}];
  (* buffer is now empty, this should now error *)
  Io_buffer.read buf 2;
  print_dyn buf;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  ("(\"not enough bytes in buffer\", { len = 2; length = 1 })") |}]
;;

let%expect_test "reading" =
  let buf = Io_buffer.create ~size:1 in
  Io_buffer.write_csexps buf [ Atom "abc" ];
  print_dyn buf;
  [%expect {|
    { total_written = 0; contents = "3:abc"; pos_w = 5; pos_r = 0 } |}];
  let flush = Io_buffer.flush_token buf in
  printfn "token: %b" (Io_buffer.flushed buf flush);
  [%expect {|
    token: false |}];
  Io_buffer.read buf 4;
  printfn "token: %b" (Io_buffer.flushed buf flush);
  [%expect {|
    token: false |}];
  Io_buffer.read buf 1;
  printfn "token: %b" (Io_buffer.flushed buf flush);
  [%expect {|
    token: true |}]
;;
