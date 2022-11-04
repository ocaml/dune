  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff a b)))
  > 
  > (rule (write-file b b))
  > EOF

  $ cat > a << EOF
  > a
  > EOF

--diff-command accepts a command name that is passed both files.

  $ dune runtest --diff-command echo
  a b
  File "a", line 1, characters 0-0:
  Error: command reported no differences: cd _build/default && echo a b
  [1]

If the command succeeds (reports no diff), this is displayed.

  $ dune runtest --diff-command true
  File "a", line 1, characters 0-0:
  Error: command reported no differences: cd _build/default && true a b
  [1]

If the command fails, this is displayed too.

  $ dune runtest --diff-command false
  File "a", line 1, characters 0-0:
  Command exited with code 1.
  [1]

As a special case, - can be passed and just the fact that they differ is
printed.

  $ dune runtest --diff-command -
  File "a", line 1, characters 0-0:
  Error: Files _build/default/a and _build/default/b differ.
  [1]

The default behavior (when --diff-command is not passed) is equivalent to
`--diff-cmd` when testing dune.

  $ dune runtest
  File "a", line 1, characters 0-0:
  Error: Files _build/default/a and _build/default/b differ.
  [1]

Outside of dune, it is to first look for patdiff in PATH.

To simulate what happens inside of dune, we'll need to setup a _path directory
with just the required binaries. _tools contains succeed (that prints its
arguments and succeeds) and fail (that prints its arguments and fails). In
addition to unsetting INSIDE_DUNE, we also need to pass
--always-show-command-line in order to have the same results locally and in CI.

  $ mkdir _tools
  $ cat > _tools/succeed << 'EOF'
  > #!/bin/sh
  > echo Running "$0" "$@"
  > EOF
  $ chmod +x _tools/succeed
  $ cat > _tools/fail << 'EOF'
  > #!/bin/sh
  > echo Running "$0" "$@"
  > false
  > EOF
  $ chmod +x _tools/fail
  $ mkdir _path
  $ ln -s $(command -v dune) _path/
  $ ln -s $(command -v ocamlc) _path/

  $ cp _tools/fail _path/patdiff
  $ cp _tools/fail _path/diff
  $ cp _tools/fail _path/git

  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  File "a", line 1, characters 0-0:
  (cd _build/default && $TESTCASE_ROOT/_path/patdiff -keep-whitespace -location-style omake -ascii a b)
  Running $TESTCASE_ROOT/_path/patdiff -keep-whitespace -location-style omake -ascii a b
  [1]

Otherwise, it will use git diff.

  $ rm _path/patdiff
  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  File "a", line 1, characters 0-0:
  $TESTCASE_ROOT/_path/git --no-pager diff --no-index --color=always -u _build/default/a _build/default/b
  Running $TESTCASE_ROOT/_path/git --no-pager diff --no-index --color=always -u _build/default/a _build/default/b
  [1]

This also happens if patdiff returns no difference.

  $ cp _tools/succeed _path/patdiff
  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  (cd _build/default && $TESTCASE_ROOT/_path/patdiff -keep-whitespace -location-style omake -ascii a b)
  Running $TESTCASE_ROOT/_path/patdiff -keep-whitespace -location-style omake -ascii a b
  File "a", line 1, characters 0-0:
  $TESTCASE_ROOT/_path/git --no-pager diff --no-index --color=always -u _build/default/a _build/default/b
  Running $TESTCASE_ROOT/_path/git --no-pager diff --no-index --color=always -u _build/default/a _build/default/b
  [1]

If patdiff or git are unavailable, it uses diff.

  $ rm _path/patdiff
  $ rm _path/git
  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  File "a", line 1, characters 0-0:
  (cd _build/default && $TESTCASE_ROOT/_path/diff -u a b)
  Running $TESTCASE_ROOT/_path/diff -u a b
  [1]

In this situation (when an automatically discovered command is used), if the
command succeeds, the "difference" message is still printed.

  $ cp _tools/succeed _path/diff
  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  (cd _build/default && $TESTCASE_ROOT/_path/diff -u a b)
  Running $TESTCASE_ROOT/_path/diff -u a b
  File "a", line 1, characters 0-0:
  Error: Files _build/default/a and _build/default/b differ.
  [1]

If diff is also not available, it just reports a difference.

  $ rm _path/diff
  $ (unset INSIDE_DUNE; PATH=_path dune runtest --always-show-command-line --root .)
  File "a", line 1, characters 0-0:
  Error: Files _build/default/a and _build/default/b differ.
  [1]
