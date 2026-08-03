Each install cookie should be deserialised at most once per build.

  $ make_lockdir

  $ make_lockpkg base <<EOF
  > (version 0.0.1)
  > (build
  >  (system "touch base.install"))
  > EOF

Two rules, both depending on package base:

  $ make_dune_project 3.24

  $ cat >dune <<EOF
  > (alias (name all) (deps (alias use1) (alias use2)))
  > (rule (alias use1) (deps (package base)) (action (write-file out1 ok)))
  > (rule (alias use2) (deps (package base)) (action (write-file out2 ok)))
  > EOF

  $ DUNE_TRACE="persistent" dune build out1 out2

Count install-cookie loads in this build. Want 1, observe 2:

  $ dune trace cat | jq -s '
  >   [ .[] | select(.args.module == "INSTALL-COOKIE" and .args.operation == "load") ]
  >   | length
  > '
  2
