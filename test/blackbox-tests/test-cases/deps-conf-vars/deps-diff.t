Deps in diff position. Used to work in 2.9.x

  $ touch foo foo.diff
  $ cat >dune <<EOF
  > (rule
  >  (deps foo)
  >  (alias foo)
  >  (action (diff? %{deps} %{deps}.diff)))
  > EOF
  $ echo "(lang dune 2.9)" > dune-project
  $ dune build @foo
  File "dune", line 4, characters 24-31:
  4 |  (action (diff? %{deps} %{deps}.diff)))
                              ^^^^^^^
  Error: %{deps} isn't allowed in this position.
  [1]
