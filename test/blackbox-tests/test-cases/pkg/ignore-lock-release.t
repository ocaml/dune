When building a project in release mode we should ignore the lock directory.

  $ . ./helpers.sh

  $ make_lockdir

  $ cat >dune.lock/test.pkg <<EOF
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

  $ dune build @install --release
