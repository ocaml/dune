Tests for promoting with symlink to non-existent file. This behaviour is tricky
to test due to the fact that INSIDE_DUNE changes the diff algo we use. The
issue appears to only happen in the internally chosen "git diff" one.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (with-stdout-to "x.gen"
  >   (echo "toto")))
  > (rule
  >  (alias bench)
  >  (action
  >   (diff promoted x.gen)))
  > EOF

Unset INSIDE_DUNE and explicitly pass --root from now on.
  $ unset INSIDE_DUNE

This should fail initially but not with the "Unable to resolve symlink" error.
  $ dune build @bench --root . 2>&1 | grep "Error"
  Error: Unable to resolve symlink _build/default/promoted

Promotion should work
  $ dune promote --root .
  Promoting _build/default/x.gen to promoted.

  $ cat promoted
  toto

