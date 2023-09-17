  $ dune build prog.exe --trace-file trace.json

This captures the commands that are being run:

  $ <trace.json grep '"X"' | cut -c 2- | sed -E 's/:[0-9]+/:.../g'
  {"args":{"dir":"."},"ph":"X","dur":...,"name":"Source tree scan","cat":"","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-config"],"pid":...},"ph":"X","dur":...,"name":"ocamlc.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-modules","-impl","prog.ml"],"pid":...},"ph":"X","dur":...,"name":"ocamldep.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-bin-annot","-I",".prog.eobjs/byte","-no-alias-deps","-opaque","-o",".prog.eobjs/byte/prog.cmo","-c","-impl","prog.ml"],"pid":...},"ph":"X","dur":...,"name":"ocamlc.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-I",".prog.eobjs/byte","-I",".prog.eobjs/native","-intf-suffix",".ml","-no-alias-deps","-opaque","-o",".prog.eobjs/native/prog.cmx","-c","-impl","prog.ml"],"pid":...},"ph":"X","dur":...,"name":"ocamlopt.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-o","prog.exe",".prog.eobjs/native/prog.cmx"],"pid":...},"ph":"X","dur":...,"name":"ocamlopt.opt","cat":"process","ts":...,"pid":...,"tid":...}
  {"args":{"path":"_build/.db","module":"INCREMENTAL-DB"},"ph":"X","dur":...,"name":"Writing Persistent Dune State","cat":"","ts":...,"pid":...,"tid":...}
  {"args":{"path":"_build/.digest-db","module":"DIGEST-DB"},"ph":"X","dur":...,"name":"Writing Persistent Dune State","cat":"","ts":...,"pid":...,"tid":...}

As well as data about the garbage collector:

  $ <trace.json grep '"C"' | cut -c 2- | sed -E 's/([^0-9])[0-9]+/\1.../g' | sort -u
  {"ph":"C","args":{"stack_size":...,"heap_words":...,"top_heap_words":...,"minor_words":...,"major_words":...,"promoted_words":...,"compactions":...,"major_collections":...,"minor_collections":...},"name":"gc","cat":"","ts":...,"pid":...,"tid":...}
  {"ph":"C","args":{"value":...},"name":"evaluated_rules","cat":"","ts":...,"pid":...,"tid":...}
  {"ph":"C","args":{"value":...},"name":"fds","cat":"","ts":...,"pid":...,"tid":...}
