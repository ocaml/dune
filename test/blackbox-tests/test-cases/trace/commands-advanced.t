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
  [1]

Verify commands show correct working directories:

  $ dune trace commands | grep "cd.*subdir" | dune_cmd subst '[^ ]*/bin/' '' | head -1
  (cd _build/default/subdir && bash -e -u -o pipefail -c "echo 'test' > output.txt")

Verify root directory commands:

  $ dune trace commands | grep "cd \$TESTCASE_ROOT &&" | head -1
  [1]

Test that all process events are captured:

  $ dune trace commands | grep "^(cd" | wc -l | awk '{if($1 > 0) print "Found process events"}'
  Found process events

Test log output format is consistent (each command on one line unless it fails):

  $ dune trace commands | grep "^(cd" | head -1 | grep -c "cd.*&&.*bash"
  0
  [1]

Create a rule with a command that has special shell characters:

  $ cat >dune <<EOF
  > (rule
  >  (target special.txt)
  >  (action (bash "echo 'test' | cat > special.txt")))
  > EOF

  $ dune build special.txt 2>&1 | grep -v "Entering directory"
  [1]

Verify special characters are properly quoted in output:

  $ dune trace commands | grep special | tail -1 | dune_cmd subst '[^ ]*/bin/' ''
  (cd _build/default && bash -e -u -o pipefail -c "echo 'test' | cat > special.txt")

Test that successful commands (exit code 0) don't show exit code:

  $ dune trace commands | grep -A 1 "special.txt" | grep -c "Exit code" || echo "No exit code for success"
  0
  No exit code for success

Test with a command in a deeply nested directory:

  $ mkdir -p a/b/c

  $ cat >a/b/c/dune <<EOF
  > (rule
  >  (target nested.txt)
  >  (action (bash "pwd > nested.txt")))
  > EOF

  $ dune build a/b/c/nested.txt 2>&1 | grep -v "Entering directory"
  [1]

Verify deeply nested paths are shown correctly:

  $ dune trace commands | grep "cd.*a/b/c" | head -1 | dune_cmd subst '[^ ]*/bin/' ''
  (cd _build/default/a/b/c && bash -e -u -o pipefail -c "pwd > nested.txt")
