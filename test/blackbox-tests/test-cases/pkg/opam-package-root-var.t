Dune cannot handle the %{root}% opam variable in an opam file. This test makes sure we
have a good error message in that case.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg with-root <<EOF
  > opam-version: "2.0"
  > build: [ "echo" root ]
  > EOF

  $ mkdir -p $mock_packages/with-root/with-root.0.0.1

Dune should reject the variable during the solving stage.

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-root))
  > EOF
  Error: Variable "root" occuring in opam package "with-root.0.0.1" is not
  supported.
  [1]
