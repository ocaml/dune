open Stdune
open Dune_tests_common

let () = init ()

let buf = Buffer.create 0

let c =
  let write s = Buffer.add_string buf s in
  let close () = () in
  Chrome_trace.make (Custom { write; close })

let () = Chrome_trace.close c

let buffer_lines () = String.split_lines (Buffer.contents buf)

let%expect_test _ =
  Format.printf "%a@." Pp.to_fmt
    (Pp.vbox (Pp.concat_map (buffer_lines ()) ~sep:Pp.cut ~f:Pp.verbatim));
  [%expect {|
]
|}]
