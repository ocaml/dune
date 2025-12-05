Applying the same setup script twice:

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > EOF

  $ cat >helpers.sh <<EOF
  > if [ -n "$x" ]; then
  >   exit 1
  > else
  >   x=1
  > fi
  > EOF

  $ cat >dune << EOF
  > (cram
  >  (setup_scripts helpers.sh))
  > (cram
  >  (setup_scripts helpers.sh))
  > EOF

  $ cat >foo.t <<'EOF'
  >   $ echo $x
  >     1
  > EOF

  $ dune runtest foo.t
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]
  $ dune promote
  Promoting _build/default/foo.t.corrected to foo.t.

  $ cat foo.t
    $ echo $x
    1
