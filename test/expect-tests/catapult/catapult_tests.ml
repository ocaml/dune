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
  Format.printf "%a@." Pp.render_ignore_tags
    (Pp.vbox (Pp.concat_map (buffer_lines ()) ~sep:Pp.cut ~f:Pp.verbatim));
  [%expect
    {|
[{"cat": "process", "name": "program", "id": 0, "pid": 0, "ph": "b", "ts": 10000000, "args": ["arg1","arg2"]}
,{"cat": "process", "name": "program", "id": 0, "pid": 0, "ph": "e", "ts": 30000000}
,{"name": "live_words", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
,{"name": "free_words", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
,{"name": "stack_size", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
,{"name": "heap_words", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
,{"name": "top_heap_words", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
,{"name": "minor_words", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0.00}}
,{"name": "major_words", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0.00}}
,{"name": "promoted_words", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0.00}}
,{"name": "compactions", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
,{"name": "major_collections", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
,{"name": "minor_collections", "pid": 0, "tid": 0, "ph": "C", "ts": 30000000, "args": {"value": 0}}
]
|}]
