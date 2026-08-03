%{bin:...} in (inline_tests (deps ...)) adds a bin-layout dir to
the test runner's PATH.

  $ make_mypkg_bin_project

Custom backend whose runner records PATH to a file outside the
sandbox:

  $ write_bin_pform_inline_tests_fixture '%{bin:mybin}'

  $ dune runtest 2>&1
  $ env_added "$(cat path.out)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
