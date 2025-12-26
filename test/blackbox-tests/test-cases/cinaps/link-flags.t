Link-time flags for running cinaps

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using cinaps 1.3)
  > EOF

  $ cat > dune <<EOF
  > (cinaps
  >  (files *.ml)
  >  (link_flags -linkall))
  > EOF

  $ touch test.ml

  $ dune build @cinaps
  $ jqScript=$(mktemp)
  $ cat >$jqScript <<EOF
  > .[] |
  > .args |
  > select(has("process_args") and (.process_args | index("-linkall"))) |
  > .process_args |
  > .[] |
  > select(endswith(".exe"))
  > EOF
  $ dune trace cat | jq -f $jqScript
  ".cinaps.edaf9873/cinaps.exe"

Check that the version guard is correct.

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using cinaps 1.3)
  > EOF

  $ dune build @cinaps
  File "dune-project", line 2, characters 14-17:
  2 | (using cinaps 1.3)
                    ^^^
  Error: Version 1.3 of the cinaps extension is not supported until version 3.8
  of the dune language.
  Supported versions of this extension in version 3.7 of the dune language:
  - 1.0 to 1.2
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using cinaps 1.2)
  > EOF

  $ dune build @cinaps
  File "dune", line 3, characters 1-22:
  3 |  (link_flags -linkall))
       ^^^^^^^^^^^^^^^^^^^^^
  Error: 'link_flags' is only available since version 1.3 of the cinaps
  extension. Please update your dune-project file to have (using cinaps 1.3).
  [1]
