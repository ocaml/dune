Requesting a file that doesn't exist in an installed package is rejected.

  $ mkdir -p src prefix consumer

  $ cat >src/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name extpkg))
  > EOF

  $ cat >src/dune <<EOF
  > (install (section share) (package extpkg) (files (src.txt as published.txt)))
  > EOF

  $ cat >src/src.txt <<EOF
  > hello from installed
  > EOF

  $ dune build --root src 2>&1

  $ dune install --root src --prefix $PWD/prefix --display short 2>&1
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/META
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/dune-package
  Installing $TESTCASE_ROOT/prefix/share/extpkg/published.txt

  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.24)
  > EOF

Requesting the published file works:

  $ cat >consumer/dune <<EOF
  > (rule
  >  (alias test-ok)
  >  (action (echo "%{pkg:extpkg:share:published.txt}\n")))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root consumer @test-ok 2>&1
  $TESTCASE_ROOT/prefix/share/extpkg/published.txt

Requesting a file that doesn't exist is rejected:

  $ cat >consumer/dune <<EOF
  > (rule
  >  (alias test-missing)
  >  (action (echo "%{pkg:extpkg:share:missing.txt}\n")))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root consumer @test-missing 2>&1
  Entering directory 'consumer'
  File "dune", line 3, characters 16-47:
  3 |  (action (echo "%{pkg:extpkg:share:missing.txt}\n")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File missing.txt not found in section share of package extpkg
  Leaving directory 'consumer'
  [1]
