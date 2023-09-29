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

Similarly for lockfiles in other contexts

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (context
  >  (default))
  > (context
  >  (default
  >   (name workspace-context)
  >   (lock dune.workspace.lock)))
  > EOF

  $ cp -r dune.lock dune.workspace.lock 
  $ cat > dune.workspace.lock/test.pkg <<EOF
  > (build
  >  (run echo "I have not been ignored from the workspace context."))
  > EOF

  $ dune build @install --release 2>&1 | tail -n1
  I have not been ignored from the workspace context.
