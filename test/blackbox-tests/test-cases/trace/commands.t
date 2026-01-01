Test dune trace commands command

Set up a simple project with rules that execute various commands:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target success.txt)
  >  (action (bash "echo 'Hello World' > success.txt")))
  >
  > (rule
  >  (target failure.txt)
  >  (deps success.txt)
  >  (action (bash "echo 'Error message' >&2 && exit 1")))
  >
  > (rule
  >  (target spaces.txt)
  >  (action (bash "echo test > spaces.txt")))
  > EOF

Build the successful target to generate trace events:

  $ dune build success.txt 2>&1 | grep -v "Entering directory"

Now test the basic trace commands output - it should show commands in shell format:

  $ dune trace commands | grep -E "^\(cd .* && " | head -3
  (cd $TESTCASE_ROOT && * (glob)
  (cd $TESTCASE_ROOT && * (glob)
  (cd $TESTCASE_ROOT && * (glob)

Verify the format is executable by checking it contains cd && pattern:

  $ dune trace commands | grep "cd.*&&.*bash" | head -1
  (cd * && * bash *) (glob)

Test with a failing command to verify stderr output:

  $ dune build failure.txt 2>&1 | head -10 || true
  * (glob)

Check that failed processes show exit code and stderr:

  $ dune trace commands | grep -A 3 "exit 1"
  (cd * && * bash *) (glob)
  # Exit code: 1
  # Stderr:
  Error message

Test the --trace-file option with a custom trace file:

  $ custom_trace="$(mktemp -d)/custom-trace.json"
  $ dune build success.txt --trace-file="$custom_trace" 2>&1 | grep -v "Entering directory"
  $ dune trace commands --trace-file="$custom_trace" | grep -E "^\(cd .* && " | head -1
  (cd $TESTCASE_ROOT && * (glob)

Test that only process events are shown (not GC, scheduler, etc.):

  $ dune trace commands | grep -v "^(cd" | grep -v "^#" | grep -v "^Error" | grep -v "^File" | wc -l
  0

Verify commands with arguments are properly formatted:

  $ dune trace commands | grep bash | head -1
  (cd * && * bash *) (glob)

Test with default trace file location:

  $ test -f _build/trace.json && echo "Default trace file exists"
  Default trace file exists

Verify the log shows actual executed commands, not dune rules:

  $ dune trace commands | grep -c "cd.*&&" | awk '{if($1 > 0) print "Has cd && format"}'
  Has cd && format
