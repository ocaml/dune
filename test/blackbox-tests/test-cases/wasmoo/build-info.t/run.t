Jsoo and build-info

  $ echo "(lang dune 3.17)" > dune-project
  $ dune build
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  unknown
  $ dune install --prefix _install --display short 2>&1 | sed 's/-[a-f0-9]*[.]wasm/.wasm/'
  Installing _install/lib/main/META
  Installing _install/lib/main/dune-package
  Installing _install/lib/main/opam
  Installing _install/bin/main
  Installing _install/bin/main.bc.wasm.assets/build_info.wasm
  Installing _install/bin/main.bc.wasm.assets/build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm
  Installing _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm.map
  Installing _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  Installing _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  Installing _install/bin/main.bc.wasm.assets/prelude.wasm
  Installing _install/bin/main.bc.wasm.assets/runtime.wasm
  Installing _install/bin/main.bc.wasm.assets/start.wasm
  Installing _install/bin/main.bc.wasm.assets/std_exit.wasm
  Installing _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  Installing _install/bin/main.bc.wasm.assets/stdlib.wasm
  Installing _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  Installing _install/bin/main.bc.wasm.js
  $ node _install/bin/main.bc.wasm.js
  unknown
  $ git init -q
  $ touch README
  $ git add README
  $ git commit -m "initial" -q
  $ git tag v1 -am "V1"
  $ git commit -m "empty2" --allow-empty -q
  $ echo "HELLO" > README
  $ dune build
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  unknown
  $ dune install --prefix _install --display short 2>&1 | sed 's/-[a-f0-9]*[.]wasm/.wasm/'
  Deleting _install/lib/main/META
  Installing _install/lib/main/META
  Deleting _install/lib/main/dune-package
  Installing _install/lib/main/dune-package
  Deleting _install/lib/main/opam
  Installing _install/lib/main/opam
  Deleting _install/bin/main
  Installing _install/bin/main
  Deleting _install/bin/main.bc.wasm.assets/build_info.wasm
  Installing _install/bin/main.bc.wasm.assets/build_info.wasm
  Deleting _install/bin/main.bc.wasm.assets/build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm
  Installing _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  Installing _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  Deleting _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  Installing _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/prelude.wasm
  Installing _install/bin/main.bc.wasm.assets/prelude.wasm
  Deleting _install/bin/main.bc.wasm.assets/runtime.wasm
  Installing _install/bin/main.bc.wasm.assets/runtime.wasm
  Deleting _install/bin/main.bc.wasm.assets/start.wasm
  Installing _install/bin/main.bc.wasm.assets/start.wasm
  Deleting _install/bin/main.bc.wasm.assets/std_exit.wasm
  Installing _install/bin/main.bc.wasm.assets/std_exit.wasm
  Deleting _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  Installing _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/stdlib.wasm
  Installing _install/bin/main.bc.wasm.assets/stdlib.wasm
  Deleting _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  Installing _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  Deleting _install/bin/main.bc.wasm.js
  Installing _install/bin/main.bc.wasm.js
  Installing _install/doc/main/README
  $ node _install/bin/main.bc.wasm.js
  v1-1-xxxxx-dirty
  $ echo "(name main)" >> dune-project
  $ echo "(version 0.2.0)" >> dune-project
  $ dune build
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  0.2.0
  $ dune install --prefix _install --display short 2>&1 | sed 's/-[a-f0-9]*[.]wasm/.wasm/'
  Deleting _install/lib/main/META
  Installing _install/lib/main/META
  Deleting _install/lib/main/dune-package
  Installing _install/lib/main/dune-package
  Deleting _install/lib/main/opam
  Installing _install/lib/main/opam
  Deleting _install/bin/main
  Installing _install/bin/main
  Deleting _install/bin/main.bc.wasm.assets/build_info.wasm
  Installing _install/bin/main.bc.wasm.assets/build_info.wasm
  Deleting _install/bin/main.bc.wasm.assets/build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm
  Installing _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  Installing _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  Deleting _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  Installing _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/prelude.wasm
  Installing _install/bin/main.bc.wasm.assets/prelude.wasm
  Deleting _install/bin/main.bc.wasm.assets/runtime.wasm
  Installing _install/bin/main.bc.wasm.assets/runtime.wasm
  Deleting _install/bin/main.bc.wasm.assets/start.wasm
  Installing _install/bin/main.bc.wasm.assets/start.wasm
  Deleting _install/bin/main.bc.wasm.assets/std_exit.wasm
  Installing _install/bin/main.bc.wasm.assets/std_exit.wasm
  Deleting _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  Installing _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/stdlib.wasm
  Installing _install/bin/main.bc.wasm.assets/stdlib.wasm
  Deleting _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  Installing _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  Deleting _install/bin/main.bc.wasm.js
  Installing _install/bin/main.bc.wasm.js
  Deleting _install/doc/main/README
  Installing _install/doc/main/README
  $ node _build/default/src/main.bc.wasm.js
  0.2.0
