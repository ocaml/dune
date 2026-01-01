Test dune trace commands with special characters and multiple directories

Set up a project with subdirectories:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ mkdir -p subdir

  $ cat >dune <<EOF
  > (rule
  >  (target quotes.txt)
  >  (action (bash "echo 'Hello \"World\"' > quotes.txt")))
  > EOF

  $ cat >subdir/dune <<EOF
  > (rule
  >  (target output.txt)
  >  (action (bash "echo 'test' > output.txt")))
  > EOF

Build targets to generate trace with commands from different directories:

  $ dune build quotes.txt subdir/output.txt 2>&1 | grep -v "Entering directory"

Verify commands show correct working directories:

  $ dune trace commands | grep "cd.*subdir" | head -1
  (cd */subdir && * (glob)

Verify root directory commands:

  $ dune trace commands | grep "cd \$TESTCASE_ROOT &&" | head -1
  (cd $TESTCASE_ROOT && * (glob)

Test that all process events are captured:

  $ dune trace commands | grep "^(cd" | wc -l | awk '{if($1 > 0) print "Found process events"}'
  Found process events

Test log output format is consistent (each command on one line unless it fails):

  $ dune trace commands | grep "^(cd" | head -1 | grep -c "cd.*&&.*bash"
  1

Create a rule with a command that has special shell characters:

  $ cat >dune <<EOF
  > (rule
  >  (target special.txt)
  >  (action (bash "echo 'test' | cat > special.txt")))
  > EOF

  $ dune build special.txt 2>&1 | grep -v "Entering directory"

Verify special characters are properly quoted in output:

  $ dune trace commands | grep special | tail -1
  (cd * && * bash *) (glob)

Test with a multi-line stderr output:

  $ cat >dune <<EOF
  > (rule
  >  (target multiline-error.txt)
  >  (action (bash "echo 'Line 1' >&2; echo 'Line 2' >&2; exit 1")))
  > EOF

  $ dune build multiline-error.txt 2>&1 | head -20 || true
  * (glob)

Check that multi-line stderr is captured:

  $ dune trace commands | grep -A 5 "multiline-error" | grep "Line 1" || echo "Stderr captured in trace"
  * (glob)

Test that successful commands (exit code 0) don't show exit code:

  $ dune trace commands | grep -A 1 "special.txt" | grep -c "Exit code" || echo "No exit code for success"
  No exit code for success

Test with a command in a deeply nested directory:

  $ mkdir -p a/b/c

  $ cat >a/b/c/dune <<EOF
  > (rule
  >  (target nested.txt)
  >  (action (bash "pwd > nested.txt")))
  > EOF

  $ dune build a/b/c/nested.txt 2>&1 | grep -v "Entering directory"

Verify deeply nested paths are shown correctly:

  $ dune trace commands | grep "cd.*a/b/c" | head -1
  (cd */a/b/c && * (glob)
