Dune cannot handle the %{root}% opam variable in an opam file. This test makes sure we
have a good error message in that case.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg with-root <<EOF
  > opam-version: "2.0"
  > build: [ "echo" root ]
  > EOF

  $ mkdir -p $mock_packages/with-root/with-root.0.0.1

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-root))
  > EOF
  Solution for dune.lock:
  with-root.0.0.1
  
Generating a lock file with this variable should give an error.

  $ cat dune.lock/with-root.pkg
  (version 0.0.1)
  
  (build
   (run echo %{pkg-self:root}))
