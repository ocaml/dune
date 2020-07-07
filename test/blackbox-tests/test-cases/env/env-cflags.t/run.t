  $ alias dune_printenv="dune printenv --profile default --field c_flags --field cxx_flags"

  $ dune_printenv .
  (c_flags (":standard + in ."))
  (cxx_flags (":standard + in ."))

  $ dune_printenv src
  (c_flags
   (":standard + in ." ":standard + in src"))
  (cxx_flags
   (":standard + in ." ":standard + in src"))

  $ dune_printenv bin
  (c_flags ("in bin"))
  (cxx_flags ("in bin"))

  $ dune_printenv run
  (c_flags (-DTEST_C))
  (cxx_flags (-DTEST_CPP))

  $ dune exec --profile default ./run/foo.exe
  TEST_C defined.

  $ dune exec --profile default ./run/bar.exe
  TEST_CPP defined.

