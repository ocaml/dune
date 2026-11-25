Tests dependencies on the full build universe.

  $ dune build @x
  Hello, world!
  $ dune build @x
  Hello, world!
  $ dune build @x
  Hello, world!
  $ dune build @x
  Hello, world!
  $ dune build @x
  Hello, world!

Two requests to the same watch server should also run the rule twice. Use a
file outside the workspace to stand in for a dependency in the installed world.

  $ input=$(mktemp)
  $ printf 'first\n' > "$input"
  $ mkdir watch
  $ cd watch
  $ make_dune_project 3.20
  $ cat > dune <<EOF
  > (rule
  >  (target result)
  >  (deps (universe) (sandbox none))
  >  (action (system "cat '$input' > result")))
  > EOF
  $ start_dune
  $ build result
  Success
  $ cat _build/default/result
  first
  $ printf 'second\n' > "$input"
  $ build result
  Success

CR: The watch server reuses the earlier result without executing the rule.

  $ cat _build/default/result
  first
  $ stop_dune_quiet

A one-shot build does execute the rule again.

  $ dune build result
  $ cat _build/default/result
  second
