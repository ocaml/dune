  $ dune build prog.exe --trace-file trace.json

This captures the commands that are being run:

  $ <trace.json grep '"X"' | cut -c 2- | sed -E 's/:[0-9]+/:.../g' | perl -pe 's/"prog":".*?"/"prog":"PROG"/g'
  {"args":{"dir":"."},"ph":"X","dur":...,"name":"Dune load: .","cat":"","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-config"],"pid":...,"categories":[],"prog":"PROG","dir":".","exit":...},"ph":"X","dur":...,"name":"ocamlc.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-modules","-impl","prog.ml"],"pid":...,"categories":[],"prog":"PROG","dir":"_build/default","exit":...,"target_files":["_build/default/.prog.eobjs/prog.impl.d"]},"ph":"X","dur":...,"name":"ocamldep.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-bin-annot","-bin-annot-occurrences","-I",".prog.eobjs/byte","-no-alias-deps","-opaque","-o",".prog.eobjs/byte/prog.cmo","-c","-impl","prog.ml"],"pid":...,"categories":[],"prog":"PROG","dir":"_build/default","exit":...,"target_files":["_build/default/.prog.eobjs/byte/prog.cmi","_build/default/.prog.eobjs/byte/prog.cmo","_build/default/.prog.eobjs/byte/prog.cmt"]},"ph":"X","dur":...,"name":"ocamlc.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-I",".prog.eobjs/byte","-I",".prog.eobjs/native","-cmi-file",".prog.eobjs/byte/prog.cmi","-no-alias-deps","-opaque","-o",".prog.eobjs/native/prog.cmx","-c","-impl","prog.ml"],"pid":...,"categories":[],"prog":"PROG","dir":"_build/default","exit":...,"target_files":["_build/default/.prog.eobjs/native/prog.cmx","_build/default/.prog.eobjs/native/prog.o"]},"ph":"X","dur":...,"name":"ocamlopt.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-o","prog.exe",".prog.eobjs/native/prog.cmx"],"pid":...,"categories":[],"prog":"PROG","dir":"_build/default","exit":...,"target_files":["_build/default/prog.exe"]},"ph":"X","dur":...,"name":"ocamlopt.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"path":"_build/.db","module":"INCREMENTAL-DB","operation":"save"},"ph":"X","dur":...,"name":"db","cat":"","ts":...,"pid":...,"tid":...}
  {"args":{"path":"_build/.digest-db","module":"DIGEST-DB","operation":"save"},"ph":"X","dur":...,"name":"db","cat":"","ts":...,"pid":...,"tid":...}

As well as data about the garbage collector:

  $ <trace.json grep '"C"' | cut -c 2- | sed -E 's/([^0-9])[0-9]+/\1.../g' | sort -u
  {"ph":"C","args":{"value":...},"name":"evaluated_rules","cat":"","ts":...,"pid":...,"tid":...}
