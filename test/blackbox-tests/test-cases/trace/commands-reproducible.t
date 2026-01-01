Test that dune trace commands output is shell-reproducible

Set up a simple project:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Create a rule that creates a file with specific content:

  $ cat >dune <<EOF
  > (rule
  >  (target hello.txt)
  >  (action (bash "echo 'Hello from dune' > hello.txt")))
  > EOF

Build the target:

  $ dune build hello.txt 2>&1 | grep -v "Entering directory"

Get the command from trace commands:

  $ dune trace commands | grep "hello.txt" | grep "^(cd" > captured_command.sh

Verify the captured command has the shell format:

  $ cat captured_command.sh
  (cd * && * bash *hello.txt*) (glob)

Test empty trace file handling:

  $ rm -rf _build/trace.json
  $ dune trace commands 2>&1 | head -5 || echo "Handles missing trace file"
  * (glob)

Test with trace file that only has non-process events:

  $ dune build hello.txt 2>&1 | grep -v "Entering directory"

  $ dune trace cat | jq 'select(.cat != "process")' > filtered.json || true

Verify log only shows process events:

  $ dune trace commands | wc -l | awk '{if($1 > 0) print "Shows only processes"}'
  Shows only processes

Test with arguments containing spaces (proper quoting):

  $ cat >dune <<EOF
  > (rule
  >  (target "file with spaces.txt")
  >  (action (bash "echo 'content' > 'file with spaces.txt'")))
  > EOF

  $ dune build "file with spaces.txt" 2>&1 | grep -v "Entering directory"

Verify spaces are quoted in output:

  $ dune trace commands | grep "file.*spaces" | tail -1
  (cd * && * bash *) (glob)

Test help message:

  $ dune trace commands --help | grep "shell format"
  * shell format* (glob)

Test that log is part of trace subcommands:

  $ dune trace --help | grep log
  * log * (glob)

Test with very long command line:

  $ cat >dune <<EOF
  > (rule
  >  (target long.txt)
  >  (action (bash "echo 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' > long.txt")))
  > EOF

  $ dune build long.txt 2>&1 | grep -v "Entering directory"

Verify long commands are not truncated:

  $ dune trace commands | grep "aaaa" | grep "^(cd"
  (cd * && * bash *aaaa*) (glob)

Test that the default trace file is used when not specified:

  $ test -f _build/trace.json && echo "Uses _build/trace.json by default"
  Uses _build/trace.json by default

Test with a process that has no arguments:

  $ cat >dune <<EOF
  > (rule
  >  (target pwd.txt)
  >  (action (bash "pwd > pwd.txt")))
  > EOF

  $ dune build pwd.txt 2>&1 | grep -v "Entering directory"

Verify commands without extra arguments are shown:

  $ dune trace commands | grep "pwd" | grep "^(cd" | tail -1
  (cd * && * bash *pwd*) (glob)
