Tests for promoting with symlink to non-existent file.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ cp dune-project dune-workspace

  $ cat > dune <<EOF
  > (rule
  >  (with-stdout-to "x.gen"
  >   (echo "toto")))
  > (rule
  >  (alias bench)
  >  (action
  >   (diff promoted x.gen)))
  > EOF

Unset INSIDE_DUNE in order to choose the "git diff" diff algorithm. Root
detection will now be automatic so a dune-workspace file was included above.
  $ unset INSIDE_DUNE

The "git diff" command that dune will run will unfortunately go looking for a
`.git` root. We therefore initialise a git repo to avoid this from escaping the
test.
  $ . ../git-helpers.sh
  $ git init -q

This should fail initially but not with the "Unable to resolve symlink" error.
  $ dune build @bench
  File "dune", lines 4-7, characters 0-55:
  4 | (rule
  5 |  (alias bench)
  6 |  (action
  7 |   (diff promoted x.gen)))
  Error: Unable to resolve symlink _build/default/promoted
  [1]

Promotion should work
  $ dune promote
  Promoting _build/default/x.gen to promoted.

  $ cat promoted
  toto

