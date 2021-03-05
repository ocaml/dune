open Stdune
open Dune_tests_common

let () = init ()

let buf = Buffer.create 0

let time = ref 0.

let c = Catapult.fake time buf

let () = time := 10.

let e =
  Catapult.on_process_start c ~program:"/path/to/program"
    ~args:[ "arg1"; "arg2" ]

let () = time := 30.

let () = Catapult.on_process_end c e

let () = Catapult.emit_gc_counters c

let () = Catapult.close c

let buffer_lines () = String.split_lines (Buffer.contents buf)

let%expect_test _ =
  Format.printf "%a@." Pp.to_fmt
    (Pp.vbox (Pp.concat_map (buffer_lines ()) ~sep:Pp.cut ~f:Pp.verbatim));
  [%expect
    {|
[{"cat": "process", "name": "program", "id": 0, "pid": 0, "ph": "b", "ts": 10000000, "args": ["arg1","arg2"]}
,{"cat": "process", "name": "program", "id": 0, "pid": 0, "ph": "e", "ts": 30000000}
,{"name": "gc", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"live_words":0,"free_words":0,"stack_size":0,"heap_words":0,"top_heap_words":0,"minor_words":0.,"major_words":0.,"promoted_words":0.,"compactions":0,"major_collections":0,"minor_collections":0}}
]
|}]
