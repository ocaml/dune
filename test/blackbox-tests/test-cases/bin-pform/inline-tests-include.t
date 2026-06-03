%{bin:NAME} inside (include ...) in an inline_tests deps field
adds the .binaries dir to the test runner's PATH, matching the
behavior of a top-level %{bin:NAME} dep (see inline-tests.t).
Dep_conf_eval.unnamed mirrors named_paths_builder's include_envs
collection to drain the env contribution from Include_result.

  $ make_mypkg_bin_project

  $ cat >deps.sexp <<'EOF'
  > (%{bin:mybin})
  > EOF

  $ write_bin_pform_inline_tests_fixture '(include deps.sexp)'

  $ dune runtest 2>&1

  $ env_added "$(cat path.out)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
