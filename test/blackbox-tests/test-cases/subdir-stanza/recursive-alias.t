Aliases defined in data_only_dirs aren't traversed

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF

  $ cat >dune<<EOF
  > (subdir foo
  >  (rule
  >   (alias run)
  >   (deps (universe))
  >   (action (echo run alias))))
  > EOF

  $ dune build @run
  run alias

  $ cat >> dune <<EOF
  > (data_only_dirs foo)
  > EOF

  $ dune build @run
  Error: Alias "run" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]
