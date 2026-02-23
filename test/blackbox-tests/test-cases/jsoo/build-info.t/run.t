Jsoo and build-info

  $ echo "(lang dune 3.0)" > dune-project
  $ dune build
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.js
  unknown
  $ dune install --prefix _install
  $ find _install -type f | sort
  _install/bin/main
  _install/bin/main.bc.js
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node _install/bin/main.bc.js
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
  Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.js
  unknown
  $ dune install --prefix _install
  $ find _install -type f | sort
  _install/bin/main
  _install/bin/main.bc.js
  _install/doc/main/README
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node _install/bin/main.bc.js
  v1-1-xxxxx-dirty
  $ echo "(name main)" >> dune-project
  $ echo "(version 0.2.0)" >> dune-project
  $ dune build
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.js
  0.2.0
  $ dune install --prefix _install
  $ find _install -type f | sort
  _install/bin/main
  _install/bin/main.bc.js
  _install/doc/main/README
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node _build/default/src/main.bc.js
  0.2.0
