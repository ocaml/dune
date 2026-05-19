%{pkg:...} resolves files installed by public executables.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (public_name foo))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline "hello"
  > EOF

The public name resolves to the build artifact:

  $ mkdir -p test

  $ cat >test/dune <<EOF
  > (rule
  >  (alias test-exe)
  >  (action (echo "%{pkg:foo:bin:foo}\n")))
  > EOF

  $ dune build @test-exe 2>&1
  ../main.exe
