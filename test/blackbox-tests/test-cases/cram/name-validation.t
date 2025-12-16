A cram test named empty.t would create the reserved 'empty' alias:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > empty.t <<EOF
  >   $ echo hello
  > EOF

  $ dune build @empty
  File "empty.t", line 1, characters 0-0:
  Error: Cram test "empty.t" would define the 'empty' alias which is reserved.
  Please rename this test file.
  [1]
