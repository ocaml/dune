Jsoo and build-info

  $ echo "(lang dune 3.0)" > dune-project
  $ dune build
  Warning: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.js
  unknown
  $ dune install --prefix _install --display short
  Installing _install/lib/main/META
  Installing _install/lib/main/dune-package
  Installing _install/lib/main/opam
  Installing _install/bin/main
  Installing _install/bin/main.bc.js
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
  Warning: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.js
  unknown
  $ dune install --prefix _install --display short
  Deleting _install/lib/main/META
  Installing _install/lib/main/META
  Deleting _install/lib/main/dune-package
  Installing _install/lib/main/dune-package
  Deleting _install/lib/main/opam
  Installing _install/lib/main/opam
  Deleting _install/bin/main
  Installing _install/bin/main
  Deleting _install/bin/main.bc.js
  Installing _install/bin/main.bc.js
  Installing _install/doc/main/README
  $ node _install/bin/main.bc.js
  v1-1-xxxxx-dirty
  $ echo "(name main)" >> dune-project
  $ echo "(version 0.2.0)" >> dune-project
  $ dune build
  Warning: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.js
  0.2.0
  $ dune install --prefix _install --display short
  Deleting _install/lib/main/META
  Installing _install/lib/main/META
  Deleting _install/lib/main/dune-package
  Installing _install/lib/main/dune-package
  Deleting _install/lib/main/opam
  Installing _install/lib/main/opam
  Deleting _install/bin/main
  Installing _install/bin/main
  Deleting _install/bin/main.bc.js
  Installing _install/bin/main.bc.js
  Deleting _install/doc/main/README
  Installing _install/doc/main/README
  $ node _build/default/src/main.bc.js
  0.2.0
