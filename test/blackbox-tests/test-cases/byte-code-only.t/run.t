  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build @all

  $ _build/default/bin-with-build-info/print_version.exe
  <version missing>

Check that building a native only executable fails
  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build native-only/foo.exe
  Error: Don't know how to build native-only/foo.exe
  [1]
