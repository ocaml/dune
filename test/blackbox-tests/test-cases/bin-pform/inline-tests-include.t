%{bin:NAME} inside (include ...) in an inline_tests deps field
adds the .binaries dir to the test runner's PATH, matching the
behavior of a top-level %{bin:NAME} dep (see inline-tests.t).
Dep_conf_eval.unnamed mirrors named_paths_builder's include_envs
collection to drain the env contribution from Include_result.

  $ make_mypkg_bin_project

  $ cat >dump_path.ml <<EOF
  > let () =
  >   let oc = open_out "$PWD/path.out" in
  >   output_string oc (Sys.getenv "PATH");
  >   close_out oc
  > EOF

  $ cat >deps.sexp <<'EOF'
  > (%{bin:mybin})
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
  >   (deps (include deps.sexp))))
  > EOF

  $ cat >testlib.ml <<EOF
  > EOF

  $ dune runtest 2>&1

  $ env_added "$(cat path.out)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
