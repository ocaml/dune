  $ dune build prog.exe --trace-file trace.json

This captures the commands that are being run:

  $ <trace.json grep '"X"' | sed 's/ [0-9]\+/ .../g'
  [{"name": "ocamlc.opt", "pid": ..., "tid": ..., "ph": "X", "dur": ..., "ts": ..., "color": "thread_state_uninterruptible", "args": ["-config"]}
  ,{"name": "ocamldep.opt", "pid": ..., "tid": ..., "ph": "X", "dur": ..., "ts": ..., "color": "thread_state_runnable", "args": ["-modules","-impl","prog.ml"]}
  ,{"name": "ocamlc.opt", "pid": ..., "tid": ..., "ph": "X", "dur": ..., "ts": ..., "color": "thread_state_uninterruptible", "args": ["-w","@a-4-29-40-41-42-44-45-48-58-59-60-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-bin-annot","-I",".prog.eobjs","-no-alias-deps","-opaque","-o",".prog.eobjs/prog.cmo","-c","-impl","prog.ml"]}
  ,{"name": "ocamlopt.opt", "pid": ..., "tid": ..., "ph": "X", "dur": ..., "ts": ..., "color": "thread_state_running", "args": ["-w","@a-4-29-40-41-42-44-45-48-58-59-60-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-I",".prog.eobjs","-intf-suffix",".ml","-no-alias-deps","-opaque","-o",".prog.eobjs/prog.cmx","-c","-impl","prog.ml"]}
  ,{"name": "ocamlopt.opt", "pid": ..., "tid": ..., "ph": "X", "dur": ..., "ts": ..., "color": "thread_state_running", "args": ["-w","@a-4-29-40-41-42-44-45-48-58-59-60-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-o","prog.exe",".prog.eobjs/prog.cmx"]}

As well as data about the garbage collector:

  $ <trace.json grep '"C"' | sed 's/ [0-9]\+/ .../g' | sort -u
  ,{"name": "free_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  ,{"name": "live_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  ,{"name": "stack_size", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
