Testing the building of executables specified by pforms in Dune.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo-pkg))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name foo))
  > EOF

  $ cat > foo.ml

dune build is able to expand pforms, but it appears that the root is incorrect.

  $ dune build %{bin:foo}
  Error: File unavailable:
  $TESTCASE_ROOT/../install/default/bin/foo
  [1]

This can be mitigated by specifying a build path:

  $ dune build _build/default/%{bin:foo}

dune exec has special support for expanding pforms for bin.

  $ dune exec -- %{bin:foo}
