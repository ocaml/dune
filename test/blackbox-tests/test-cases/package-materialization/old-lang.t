Depending on a local package should work before Dune language 2.9, even when
one of its libraries depends on an installed library such as unix. The version
restriction applies only to explicit dependencies on installed packages.

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
  ok
