Testing dune #10674 where an empty version in an opam file caused a code error
in dune. We should make sure that this case is handled gracefully.

  $ dune build
  File "foo.opam", line 2, characters 0-7:
  2 | version: ""
      ^^^^^^^
  Error: "" is an invalid package version.
  [1]
