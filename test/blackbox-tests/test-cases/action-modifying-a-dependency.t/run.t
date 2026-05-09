In this test the "x" alias depends on the file "data" but the action associated
to "x" appends a line to "data". Dune should eventually reject this, but for now
the test documents that source-copy dependencies are refreshed from the source
tree before the alias action runs.

  $ echo hello > data
  $ dune build @x
  $ cat _build/default/data
  hello
  hello

Rebuilding refreshes the source-copy dependency from the source tree before the
alias action runs again, so the build-directory copy does not keep accumulating
appended lines.

  $ dune build @x
  $ cat _build/default/data
  hello
  hello
