  $ dune build prog.exe --trace-file trace.json

This captures the commands that are being run:

  $ <trace.json grep '"[be]"' | cut -c 2- | sed -E 's/ [0-9]+/ .../g'
  {"args":{"process_args":["-config"]},"ph":"b","id":0,"name":"ocamlc.opt","cat":"process","ts":1615007975704577,"pid":0,"tid":0}
  {"ph":"e","id":0,"name":"ocamlc.opt","cat":"process","ts":1615007975729771,"pid":0,"tid":0}
  {"args":{"process_args":["-modules","-impl","prog.ml"]},"ph":"b","id":1,"name":"ocamldep.opt","cat":"process","ts":1615007975755344,"pid":0,"tid":0}
  {"ph":"e","id":1,"name":"ocamldep.opt","cat":"process","ts":1615007975765688,"pid":0,"tid":0}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-bin-annot","-I",".prog.eobjs/byte","-no-alias-deps","-opaque","-o",".prog.eobjs/byte/prog.cmo","-c","-impl","prog.ml"]},"ph":"b","id":2,"name":"ocamlc.opt","cat":"process","ts":1615007975787361,"pid":0,"tid":0}
  {"ph":"e","id":2,"name":"ocamlc.opt","cat":"process","ts":1615007975805999,"pid":0,"tid":0}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-I",".prog.eobjs/byte","-I",".prog.eobjs/native","-intf-suffix",".ml","-no-alias-deps","-opaque","-o",".prog.eobjs/native/prog.cmx","-c","-impl","prog.ml"]},"ph":"b","id":3,"name":"ocamlopt.opt","cat":"process","ts":1615007975834049,"pid":0,"tid":0}
  {"ph":"e","id":3,"name":"ocamlopt.opt","cat":"process","ts":1615007975919340,"pid":0,"tid":0}
  {"args":{"process_args":["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-o","prog.exe",".prog.eobjs/native/prog.cmx"]},"ph":"b","id":4,"name":"ocamlopt.opt","cat":"process","ts":1615007975923300,"pid":0,"tid":0}
  {"ph":"e","id":4,"name":"ocamlopt.opt","cat":"process","ts":1615007976122260,"pid":0,"tid":0}

As well as data about the garbage collector:

  $ <trace.json grep '"C"' | cut -c 2- | sed -E 's/([^0-9])[0-9]+/\1.../g' | sort -u
  {"ph":"C","args":{"live_words":...,"free_words":...,"stack_size":...,"heap_words":...,"top_heap_words":...,"minor_words":....,"major_words":....,"promoted_words":....,"compactions":...,"major_collections":...,"minor_collections":...},"name":"gc","cat":"","ts":...,"pid":...,"tid":...}
  {"ph":"C","args":{"value":...},"name":"evaluated_rules","cat":"","ts":...,"pid":...,"tid":...}
  {"ph":"C","args":{"value":...},"name":"fds","cat":"","ts":...,"pid":...,"tid":...}
