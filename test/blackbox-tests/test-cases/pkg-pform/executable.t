%{pkg:...} resolves files installed by public executables.

  $ make_dune_project_with_package 3.24 foo

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
