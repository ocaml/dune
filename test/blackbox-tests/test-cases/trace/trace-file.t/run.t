  $ export DUNE_TRACE="process,gc"
  $ dune build prog.exe

This captures the commands that are being run:

  $ dune trace cat | jq 'include "dune";
  >   processes
  > | .args
  > | del(.pid)
  > | .prog |= sub(".*/"; "")
  > | .rusage |= keys
  > '
  {
    "process_args": [
      "-config"
    ],
    "categories": [],
    "prog": "ocamlc.opt",
    "dir": ".",
    "exit": 0,
    "rusage": [
      "inblock",
      "majflt",
      "maxrss",
      "minflt",
      "nivcsw",
      "nvcsw",
      "oublock",
      "system_cpu_time",
      "user_cpu_time"
    ]
  }
  {
    "process_args": [
      "-modules",
      "-impl",
      "prog.ml"
    ],
    "categories": [],
    "prog": "ocamldep.opt",
    "dir": "_build/default",
    "exit": 0,
    "target_files": [
      "_build/default/.prog.eobjs/prog.impl.d"
    ],
    "rusage": [
      "inblock",
      "majflt",
      "maxrss",
      "minflt",
      "nivcsw",
      "nvcsw",
      "oublock",
      "system_cpu_time",
      "user_cpu_time"
    ]
  }
  {
    "process_args": [
      "-w",
      "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
      "-strict-sequence",
      "-strict-formats",
      "-short-paths",
      "-keep-locs",
      "-g",
      "-bin-annot",
      "-bin-annot-occurrences",
      "-I",
      ".prog.eobjs/byte",
      "-no-alias-deps",
      "-opaque",
      "-o",
      ".prog.eobjs/byte/prog.cmo",
      "-c",
      "-impl",
      "prog.ml"
    ],
    "categories": [],
    "prog": "ocamlc.opt",
    "dir": "_build/default",
    "exit": 0,
    "target_files": [
      "_build/default/.prog.eobjs/byte/prog.cmi",
      "_build/default/.prog.eobjs/byte/prog.cmo",
      "_build/default/.prog.eobjs/byte/prog.cmt"
    ],
    "rusage": [
      "inblock",
      "majflt",
      "maxrss",
      "minflt",
      "nivcsw",
      "nvcsw",
      "oublock",
      "system_cpu_time",
      "user_cpu_time"
    ]
  }
  {
    "process_args": [
      "-w",
      "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
      "-strict-sequence",
      "-strict-formats",
      "-short-paths",
      "-keep-locs",
      "-g",
      "-I",
      ".prog.eobjs/byte",
      "-I",
      ".prog.eobjs/native",
      "-cmi-file",
      ".prog.eobjs/byte/prog.cmi",
      "-no-alias-deps",
      "-opaque",
      "-o",
      ".prog.eobjs/native/prog.cmx",
      "-c",
      "-impl",
      "prog.ml"
    ],
    "categories": [],
    "prog": "ocamlopt.opt",
    "dir": "_build/default",
    "exit": 0,
    "target_files": [
      "_build/default/.prog.eobjs/native/prog.cmx",
      "_build/default/.prog.eobjs/native/prog.o"
    ],
    "rusage": [
      "inblock",
      "majflt",
      "maxrss",
      "minflt",
      "nivcsw",
      "nvcsw",
      "oublock",
      "system_cpu_time",
      "user_cpu_time"
    ]
  }
  {
    "process_args": [
      "-w",
      "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
      "-strict-sequence",
      "-strict-formats",
      "-short-paths",
      "-keep-locs",
      "-g",
      "-o",
      "prog.exe",
      ".prog.eobjs/native/prog.cmx"
    ],
    "categories": [],
    "prog": "ocamlopt.opt",
    "dir": "_build/default",
    "exit": 0,
    "target_files": [
      "_build/default/prog.exe"
    ],
    "rusage": [
      "inblock",
      "majflt",
      "maxrss",
      "minflt",
      "nivcsw",
      "nvcsw",
      "oublock",
      "system_cpu_time",
      "user_cpu_time"
    ]
  }

As well as data about the garbage collector:

  $ dune trace cat | jq -s '[ .[] | select(.cat == "gc") ] | .[0] | .args | keys'
  [
    "compactions",
    "heap_words",
    "major_collections",
    "major_words",
    "minor_collections",
    "minor_words",
    "promoted_words",
    "stack_size",
    "top_heap_words"
  ]
