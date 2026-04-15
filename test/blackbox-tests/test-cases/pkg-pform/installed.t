Test that %{pkg:...} works for installed packages outside the workspace.

  $ mkdir -p src prefix consumer

  $ cat >src/dune-project <<EOF
  > (lang dune 3.23)
  > (package (name extpkg))
  > EOF

  $ cat >src/dune <<EOF
  > (install (section share) (package extpkg) (files (hello.txt as hello.txt)))
  > EOF

  $ cat >src/hello.txt <<EOF
  > hello from installed
  > EOF

  $ dune build --root src 2>&1
  Entering directory 'src'
  Leaving directory 'src'

  $ dune install --root src --prefix $PWD/prefix --display short 2>&1
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/META
  Installing $TESTCASE_ROOT/prefix/lib/extpkg/dune-package
  Installing $TESTCASE_ROOT/prefix/share/extpkg/hello.txt

Reference the installed package from a separate project via OCAMLPATH.
The share section resolves to the absolute installed path:

  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat >consumer/dune <<EOF
  > (rule
  >  (alias test-installed-pkg)
  >  (action (echo "share: %{pkg:extpkg:share}\n")))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root consumer @test-installed-pkg 2>&1
  Entering directory 'consumer'
  share: $TESTCASE_ROOT/prefix/share/extpkg
  Leaving directory 'consumer'

A dependency on the package's lib directory is registered:

  $ cat >consumer/dune <<EOF
  > (rule
  >  (target output)
  >  (action (with-stdout-to %{target} (echo %{pkg:extpkg:share}))))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune rules --root consumer --deps _build/default/output 2>&1 | grep -F prefix/lib/extpkg
     $TESTCASE_ROOT/prefix/lib/extpkg)))

The installed artifact is accessible through the expanded path:

  $ cat >consumer/dune <<EOF
  > (rule
  >  (alias test-read)
  >  (action (system "cat %{pkg:extpkg:share}/hello.txt")))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root consumer @test-read 2>&1
  Entering directory 'consumer'
  hello from installed
  Leaving directory 'consumer'
