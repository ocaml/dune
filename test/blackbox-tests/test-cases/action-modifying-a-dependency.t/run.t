In this test the "x" alias depends on the file "data" but the action
associated to "x" appends a line to "data". The expected behavior is
an error from Dune telling the user that this is not allowed, however
Dune currently silently ignores this.

  $ echo hello > data
  $ dune build @x
  $ cat _build/default/data
  hello
  hello

  $ dune build @x
  $ cat _build/default/data
  hello
  hello

  $ dune build @x
  $ cat _build/default/data
  hello
  hello
