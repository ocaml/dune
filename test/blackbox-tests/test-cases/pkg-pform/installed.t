Test that %{pkg:...} works for installed packages outside the workspace.

  $ mkdir -p src prefix consumer

  $ cat >src/dune-project <<EOF
  > (lang dune 3.23)
  > (package (name extpkg))
  > EOF

  $ cat >src/dune <<EOF
  > (install (section share) (package extpkg) (files (src.txt as published.txt)))
  > EOF

  $ cat >src/src.txt <<EOF
  > hello from installed
  > EOF

  $ dune build --root src 2>&1
  Entering directory 'src'
  Leaving directory 'src'

  $ dune install --root src --prefix $PWD/prefix --display short 2>&1
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/META
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/dune-package
  Installing $TESTCASE_ROOT/prefix/share/extpkg/published.txt

Reference the installed file from a separate project via OCAMLPATH:

  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat >consumer/dune <<EOF
  > (rule
  >  (alias test-installed-file)
  >  (action (echo "%{pkg:extpkg:share:published.txt}\n")))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root consumer @test-installed-file 2>&1
  Entering directory 'consumer'
  $TESTCASE_ROOT/prefix/share/extpkg/published.txt
  Leaving directory 'consumer'
