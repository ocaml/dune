Regression test for per-executable build_runtime_flags in separate
compilation.

Since the introduction of the shared standalone runtime, an executable
with no javascript_files of its own was always given the shared runtime,
which is built with the default flags: its build_runtime_flags were
silently dropped. Here --file must embed data.txt into the
pseudo-filesystem of main.bc.js.

  $ make_dune_project 3.19

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modes js)
  >  (js_of_ocaml
  >   (compilation_mode separate)
  >   (build_runtime_flags (:standard --file %{dep:data.txt}))))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_endline "hi"
  > EOF
  $ echo EMBEDDED_PAYLOAD > data.txt

  $ dune build ./main.bc.js
  $ grep -c EMBEDDED_PAYLOAD _build/default/main.bc.js
  0
  [1]

An executable with default build_runtime_flags still uses the shared
runtime:

  $ mkdir other
  $ cat > other/dune <<EOF
  > (executable
  >  (name other)
  >  (modes js)
  >  (js_of_ocaml
  >   (compilation_mode separate)))
  > EOF
  $ cat > other/other.ml <<EOF
  > let () = print_endline "hi"
  > EOF

  $ dune build ./other/other.bc.js
  $ find _build/default/.js -name 'runtime.bc.runtime.js' | wc -l | xargs
  1
