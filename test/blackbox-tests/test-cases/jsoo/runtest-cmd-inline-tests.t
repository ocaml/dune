Test running js_of_ocaml inline tests by specifying ML source files directly.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name fake_backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner
  >    (progn
  >     (echo "[@@@warning \\"-40\\"]\\n")
  >     (cat %{impl-files})))))
  > 
  > (library
  >  (name inline_tests_jsoo)
  >  (modules jsoo_lib)
  >  (js_of_ocaml (javascript_files jsoo.js))
  >  (inline_tests (modes js) (backend fake_backend)))
  > EOF

  $ cat > jsoo_lib.ml <<EOF
  > external prim : unit -> bool = "jsoo_stubs"
  > let _ = assert (Sys.int_size = 32)
  > let _ = assert (prim ())
  > let _ = print_endline "inline tests (JS)"
  > let _ = assert false
  > EOF

  $ cat > jsoo.js <<EOF
  > //Provides: jsoo_stubs
  > function jsoo_stubs(unit) {
  >   return 1;
  > }
  > EOF

Running js_of_ocaml inline tests by specifying the ML file:

  $ dune test jsoo_lib.ml
  Warning [missing-effects-backend]: your program contains effect handlers; you should probably run js_of_ocaml with option '--effects=cps'
  File "dune", line 14, characters 1-49:
  14 |  (inline_tests (modes js) (backend fake_backend)))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  inline tests (JS)
  Fatal error: exception File "jsoo_lib.ml", line 5, characters 8-14: Assertion failed
  [1]
