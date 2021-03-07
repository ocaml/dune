open Stdune
open Dune_tests_common

let () = init ()

let buf = Buffer.create 0

let time = ref 0.

let c = Chrome_trace.fake time buf

let () = time := 10.

let () = time := 30.

let () = Chrome_trace.close c

let buffer_lines () = String.split_lines (Buffer.contents buf)

let%expect_test _ =
  Format.printf "%a@." Pp.to_fmt
    (Pp.vbox (Pp.concat_map (buffer_lines ()) ~sep:Pp.cut ~f:Pp.verbatim));
  [%expect {|
]
|}]
