%{bin:...} in (inline_tests (deps ...)) adds the install bin dir to
the test runner's PATH.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > EOF
  $ cat >src/mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

Custom backend whose runner records PATH to a file outside the
sandbox:

  $ cat >dump_path.ml <<EOF
  > let () =
  >   let oc = open_out "$PWD/path.out" in
  >   output_string oc (Sys.getenv "PATH");
  >   close_out oc
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (name check_backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (cat dump_path.ml))))
  > (library
  >  (name testlib)
  >  (inline_tests
  >   (backend check_backend)
  >   (deps %{bin:mybin})))
  > EOF

  $ cat >testlib.ml <<EOF
  > EOF

  $ dune runtest 2>&1
  $ env_added "$(cat path.out)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
  $PWD/_build/install/default/bin
