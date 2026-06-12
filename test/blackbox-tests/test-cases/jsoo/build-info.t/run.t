Jsoo and build-info

  $ make_dune_project 3.0
  $ js=src/main.bc.js
  $ built_js=_build/default/$js
  $ installed_js=_install/bin/main.bc.js
  $ dune build "$js" @install
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Consider passing '-g' option to ocamlc.
  $ node "$built_js"
  unknown
  $ dune install --prefix _install
  $ find _install -type f | sort
  _install/bin/main
  _install/bin/main.bc.js
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node "$installed_js"
  unknown
  $ git init -q
  $ touch README
  $ git add README
  $ git commit -m "initial" -q
  $ git tag v1 -am "V1"
  $ git commit -m "empty2" --allow-empty -q
  $ echo "HELLO" > README
  $ dune build "$js" @install
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Consider passing '-g' option to ocamlc.
  $ node "$built_js"
  unknown
  $ dune install --prefix _install
  $ find _install -type f | sort
  _install/bin/main
  _install/bin/main.bc.js
  _install/doc/main/README
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node "$installed_js"
  v1-1-xxxxx-dirty
  $ echo "(name main)" >> dune-project
  $ echo "(version 0.2.0)" >> dune-project
  $ dune build "$js" @install
  Warning [missing-debug-event]: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Consider passing '-g' option to ocamlc.
  $ node "$built_js"
  0.2.0
  $ dune install --prefix _install
  $ find _install -type f | sort
  _install/bin/main
  _install/bin/main.bc.js
  _install/doc/main/README
  _install/lib/main/META
  _install/lib/main/dune-package
  _install/lib/main/opam
  $ node "$built_js"
  0.2.0
