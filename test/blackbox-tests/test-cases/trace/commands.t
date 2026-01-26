Test dune trace commands command

Set up a simple project with rules that execute various commands:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target success.txt)
  >  (action (system "echo 'Hello World' > success.txt")))
  > 
  > (rule
  >  (target failure.txt)
  >  (deps success.txt)
  >  (action (system "echo 'Error message' >&2 && exit 1")))
  > 
  > (rule
  >  (target spaces.txt)
  >  (action (system "echo test > spaces.txt")))
  > EOF

Build the successful target to generate trace events:

  $ dune build success.txt

Now test the basic trace commands output - it should show commands in shell format:

  $ dune trace commands | grep -E "^\(cd .* && " | head -3 | dune_cmd subst '[^ ]*/bin/' ''
  (cd . && ocamlc.opt -config)
  (cd _build/default && sh -c "echo 'Hello World' > success.txt")

Verify the format is executable by checking it contains cd && pattern:

  $ dune trace commands | grep "cd.*&&.*sh" | dune_cmd subst '[^ ]*/bin/' '' | head -1
  (cd _build/default && sh -c "echo 'Hello World' > success.txt")

Test with a failing command to verify stderr output:

  $ dune build failure.txt
  File "dune", lines 5-8, characters 0-104:
  5 | (rule
  6 |  (target failure.txt)
  7 |  (deps success.txt)
  8 |  (action (system "echo 'Error message' >&2 && exit 1")))
  Error message
  [1]

Check that failed processes show exit code and stderr:

  $ dune trace commands | grep -A 3 "exit 1" | dune_cmd subst '[^ ]*/bin/' ''
  (cd _build/default && sh -c "echo 'Error message' >&2 && exit 1")
  # exited with code 1
  # Stderr:
  Error message
