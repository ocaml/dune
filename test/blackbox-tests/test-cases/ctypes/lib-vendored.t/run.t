Generate cstubs for a "vendored" library.

We have a dummy C library hosted entirely in the 'vendor' directory and use
the ctypes instrumentation and description language to generate bindings for
it.

This is the version that builds into a library.
  $ LIBEX=$(realpath "$PWD/../libexample")
  $ TARGET=./stubgen/vendor
  $ mkdir -p $TARGET && install $LIBEX/*example* $TARGET
  $ DYLD_LIBRARY_PATH="$TARGET" LD_LIBRARY_PATH="$TARGET" dune exec ./example.exe
  4

With Ctypes 0.3, the C stubs library link runs outside a sandbox.

  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select((.args.target_files // [])
  >            | any(contains("dllexamplelib_stubs")))
  >   | { sandbox: (.args.dir | contains(".sandbox"))
  >     , example_flags:
  >         ([.args.process_args[] | select(. == "-lexample")] | length)
  >     }
  >   ][0]'
  {"sandbox":false,"example_flags":1}

Ctypes 0.4 introduces link dependencies and requires the C stubs library link
to be sandboxed.

  $ sed -i.bak 's/(using ctypes 0.3)/(using ctypes 0.4)/' dune-project
  $ sed -i.bak \
  >   's/; LINK_DEPS/(link_deps (source_tree vendor))/' stubgen/dune
  $ rm dune-project.bak stubgen/dune.bak
  $ DYLD_LIBRARY_PATH="$TARGET" LD_LIBRARY_PATH="$TARGET" dune exec ./example.exe
  4
  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select((.args.target_files // [])
  >            | any(contains("dllexamplelib_stubs")))
  >   | { sandbox: (.args.dir | contains(".sandbox"))
  >     , example_flags:
  >         ([.args.process_args[] | select(. == "-lexample")] | length)
  >     }
  >   ][0]'
  {"sandbox":true,"example_flags":1}

