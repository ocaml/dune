Test that (inline_tests (deps (package ...))) sets up layout env vars
for the test runner.

  $ make_mypkg_lib_project

Custom backend that writes OCAMLPATH to a file outside the sandbox:

  $ cat >dump_ocamlpath.ml <<EOF
  > let () =
  >   let oc = open_out "$PWD/ocamlpath.out" in
  >   output_string oc (Sys.getenv "OCAMLPATH");
  >   close_out oc
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (name check_backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (cat dump_ocamlpath.ml))))
  > (library
  >  (name testlib)
  >  (inline_tests
  >   (backend check_backend)
  >   (deps (package mypkg))))
  > EOF

  $ cat >testlib.ml <<EOF
  > EOF

  $ baseline=$OCAMLPATH
  $ dune runtest 2>&1
  $ env_added "$(cat ocamlpath.out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib
