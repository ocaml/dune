Reports unreadable or broken targets after a rule runs.

  $ make_dune_project 2.9

  $ cat > dune <<EOF
  > (rule
  >   (targets a b)
  >   (action (bash "echo content > a; chmod -r a; ln -s foo b")))
  > EOF

  $ dune build b
  File "dune", lines 1-3, characters 0-84:
  1 | (rule
  2 |   (targets a b)
  3 |   (action (bash "echo content > a; chmod -r a; ln -s foo b")))
  Error: Error trying to read targets after a rule was run:
  - a: open(_build/default/a): Permission denied
  - b: Broken symbolic link
  [1]

Report the rule location for copy actions that fail while executing.

  $ mkdir copy-error
  $ cd copy-error
  $ make_dune_project 3.24
  $ mkdir -p sub
  $ echo '{}' > sub/file.json
  $ cat > dune <<EOF
  > (rule
  >  (target f.json)
  >  (action
  >   (with-stdout-to %{target}
  >    (copy sub/file.json f.json))))
  > EOF

  $ dune build 2>&1 \
  > | sed -E 's#_build/\.sandbox/[0-9a-f]+#_build/.sandbox/$SANDBOX#'
  File "dune", lines 1-5, characters 0-93:
  1 | (rule
  2 |  (target f.json)
  3 |  (action
  4 |   (with-stdout-to %{target}
  5 |    (copy sub/file.json f.json))))
  Error:
  open(_build/.sandbox/$SANDBOX/default/f.json): Permission denied
  [1]
