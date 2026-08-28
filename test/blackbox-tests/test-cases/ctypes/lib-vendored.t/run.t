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
  >   [.[] | processes] as $processes
  >   | { sandbox:
  >         ([$processes[]
  >          | select((.args.target_files // [])
  >                   | any(contains("dllexamplelib_stubs")))
  >          | (.args.dir | contains(".sandbox"))
  >          ][0])
  >     , example_flags:
  >         ([$processes[]
  >          | select((.args.target_files // [])
  >                   | index("_build/default/stubgen/examplelib.cmxa"))
  >          | [.args.process_args[] | select(. == "-lexample")] | length
  >          ][0])
  >     }'
  {"sandbox":false,"example_flags":1}

