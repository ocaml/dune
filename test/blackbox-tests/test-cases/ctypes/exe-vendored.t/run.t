Generate cstubs for a "vendored" library.

We have a dummy C library hosted entirely in the 'vendor' directory and use
the ctypes instrumentation and description language to generate bindings for
it.

This is the version that builds into an executable.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ TARGET=./vendor
  $ mkdir -p $TARGET && install $LIBEX/*example* $TARGET
Ctypes stub generation is sandboxed and honors its declared dependencies.

  $ DYLD_LIBRARY_PATH="$TARGET" LD_LIBRARY_PATH="$TARGET" dune exec ./example.exe
  4
  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select((.args.target_files // [])
  >            | any(startswith("_build/default/examplelib__")))
  >   | (.args.dir | contains(".sandbox"))
  >   ] | unique'
  [true]

With Ctypes 0.3, the final executable link runs outside a sandbox.

  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select((.args.target_files // [])
  >            | index("_build/default/example.exe"))
  >   | { sandbox: (.args.dir | contains(".sandbox"))
  >     , example_flags:
  >         ([.args.process_args[] | select(. == "-lexample")] | length)
  >     }
  >   ][0]'
  {"sandbox":false,"example_flags":1}

The Ctypes headers found through its implicit include directory are rule
dependencies.

  $ dune rules --format=json \
  >   _build/default/examplelib__c_cout_generated_types.exe \
  > | jq_dune -c '
  >   ([ .[]
  >    | select(.deps)
  >    | ruleDepFilePaths
  >    | select(endswith("ctypes_cstubs_internals.h"))
  >    | {kind: "file", path: basename}
  >    ]
  >    +
  >    [ .[]
  >    | select(.deps)
  >    | ruleDepGlobEntries
  >    | select((.dir | endswith("ctypes"))
  >             and (.predicate | tostring | contains(".h")))
  >    | {kind: "glob", dir: (.dir | basename), predicate}
  >    ]) | unique'
  [{"kind":"glob","dir":"ctypes","predicate":"*.h"}]

Ctypes 0.4 introduces link dependencies and requires the final link to be
sandboxed.

  $ sed -i.bak 's/(using ctypes 0.3)/(using ctypes 0.4)/' dune-project
  $ sed -i.bak 's/; LINK_DEPS/(link_deps (source_tree vendor))/' dune
  $ rm dune-project.bak dune.bak
  $ DYLD_LIBRARY_PATH="$TARGET" LD_LIBRARY_PATH="$TARGET" dune exec ./example.exe
  4
  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select((.args.target_files // [])
  >            | index("_build/default/example.exe"))
  >   | { sandbox: (.args.dir | contains(".sandbox"))
  >     , example_flags:
  >         ([.args.process_args[] | select(. == "-lexample")] | length)
  >     }
  >   ][0]'
  {"sandbox":true,"example_flags":1}
