Jsoo and build-info

  $ echo "(lang dune 3.17)" > dune-project
  $ dune build
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  unknown
  $ dune install --prefix _install
  $ find _install -type f | sed 's/-[a-f0-9]*[.]wasm/.wasm/' | sort -u
  _install/bin/main
  _install/bin/main.bc.wasm.assets/build_info.wasm
  _install/bin/main.bc.wasm.assets/build_info.wasm.map
  _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm
  _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm.map
  _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  _install/bin/main.bc.wasm.assets/prelude.wasm
  _install/bin/main.bc.wasm.assets/runtime.wasm
  _install/bin/main.bc.wasm.assets/start.wasm
  _install/bin/main.bc.wasm.assets/std_exit.wasm
  _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  _install/bin/main.bc.wasm.assets/stdlib.wasm
  _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  _install/bin/main.bc.wasm.js
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
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
  $ dune install --prefix _install
  $ find _install -type f | sed 's/-[a-f0-9]*[.]wasm/.wasm/' | sort -u
  _install/bin/main
  _install/bin/main.bc.wasm.assets/build_info.wasm
  _install/bin/main.bc.wasm.assets/build_info.wasm.map
  _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm
  _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm.map
  _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  _install/bin/main.bc.wasm.assets/prelude.wasm
  _install/bin/main.bc.wasm.assets/runtime.wasm
  _install/bin/main.bc.wasm.assets/start.wasm
  _install/bin/main.bc.wasm.assets/std_exit.wasm
  _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  _install/bin/main.bc.wasm.assets/stdlib.wasm
  _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  _install/bin/main.bc.wasm.js
  _install/doc/main/README
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node _install/bin/main.bc.wasm.js
  v1-1-xxxxx-dirty
  $ echo "(name main)" >> dune-project
  $ echo "(version 0.2.0)" >> dune-project
  $ dune build
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  0.2.0
  $ dune install --prefix _install
  $ find _install -type f | sed 's/-[a-f0-9]*[.]wasm/.wasm/' | sort -u
  _install/bin/main
  _install/bin/main.bc.wasm.assets/build_info.wasm
  _install/bin/main.bc.wasm.assets/build_info.wasm.map
  _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm
  _install/bin/main.bc.wasm.assets/build_info__Build_info_data.wasm.map
  _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm
  _install/bin/main.bc.wasm.assets/dune__exe__Main.wasm.map
  _install/bin/main.bc.wasm.assets/prelude.wasm
  _install/bin/main.bc.wasm.assets/runtime.wasm
  _install/bin/main.bc.wasm.assets/start.wasm
  _install/bin/main.bc.wasm.assets/std_exit.wasm
  _install/bin/main.bc.wasm.assets/std_exit.wasm.map
  _install/bin/main.bc.wasm.assets/stdlib.wasm
  _install/bin/main.bc.wasm.assets/stdlib.wasm.map
  _install/bin/main.bc.wasm.js
  _install/doc/main/README
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node _build/default/src/main.bc.wasm.js
  0.2.0
