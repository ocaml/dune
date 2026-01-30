Test behavior of disabled test stanzas.

Unlike cram tests, disabled test stanzas cannot be run explicitly.
The enabled_if field completely prevents the test from running.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (test
  >  (name disabled_test)
  >  (enabled_if false))
  > EOF

  $ cat > disabled_test.ml <<EOF
  > let () = print_endline "test ran"
  > EOF

The disabled test is skipped from @runtest:

  $ dune runtest

Trying to run it explicitly via its alias errors - the alias doesn't exist:

  $ dune build @runtest-disabled_test
  Error: Alias "runtest-disabled_test" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]
