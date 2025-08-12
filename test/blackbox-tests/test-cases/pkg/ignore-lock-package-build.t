When building a project with -p we should ignore the lock directory. This is so
that packages with lockdirs in their source archive can be built by opam
without using locked dependencies.

  $ . ./helpers.sh

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (build
  >  (run echo "I have not been ignored."))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends test))
  > EOF

  $ dune build @install -p foo
