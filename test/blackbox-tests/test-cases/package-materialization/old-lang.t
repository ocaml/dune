Depending on a local package should work before Dune language 2.9, even when
one of its libraries depends on an installed library such as unix. Currently,
following those library dependencies incorrectly applies the version restriction
for explicit dependencies on installed packages.

  $ make_dune_project 2.0
  $ touch foo.opam
  $ echo 'let pid = Unix.getpid' >foo.ml
  $ cat >dune <<'EOF'
  > (library
  >  (public_name foo)
  >  (libraries unix))
  > (rule
  >  (alias runtest)
  >  (deps (package foo))
  >  (action (echo ok)))
  > EOF
  $ dune runtest
  Error: Dependency on an installed package requires at least (lang dune 2.9)
  -> required by alias runtest in dune:4
  [1]

The same dependency succeeds with language version 2.9.

  $ make_dune_project 2.9
  $ dune runtest --build-dir _build_2_9
  ok
