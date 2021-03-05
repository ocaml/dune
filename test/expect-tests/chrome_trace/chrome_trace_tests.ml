open Stdune
open Dune_tests_common

let () = init ()

let buf = Buffer.create 0

let time = ref 0.

let c = Chrome_trace.fake time buf

let () = time := 10.

let e =
  Chrome_trace.on_process_start c ~program:"/path/to/program"
    ~args:[ "arg1"; "arg2" ]

let () = time := 30.

let () = Chrome_trace.on_process_end c e

let () = Chrome_trace.emit_gc_counters c

let () = Chrome_trace.close c

let buffer_lines () = String.split_lines (Buffer.contents buf)

let%expect_test _ =
  Format.printf "%a@." Pp.to_fmt
    (Pp.vbox (Pp.concat_map (buffer_lines ()) ~sep:Pp.cut ~f:Pp.verbatim));
  [%expect
    {|
[{"args":{"process_args":["arg1","arg2"]},"ph":"b","id":0,"name":"program","cat":"process","ts":10000000,"pid":0,"tid":0}
,{"ph":"b","id":0,"name":"program","cat":"process","ts":30000000,"pid":0,"tid":0}
,{"ph":"C","args":{"live_words":0,"free_words":0,"stack_size":0,"heap_words":0,"top_heap_words":0,"minor_words":0.,"major_words":0.,"promoted_words":0.,"compactions":0,"major_collections":0,"minor_collections":0},"name":"gc","cat":"","ts":30000000,"pid":0,"tid":0}
]
|}]
