Duplicate repository names defined in the same workspace file:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (repository
  >  (name foo)
  >  (url "git+file//foo"))
  > (repository
  >  (name foo)
  >  (url "git+file//foo"))
  > EOF

  $ dune build
  File "dune-workspace", line 7, characters 6-21:
  7 |  (url "git+file//foo"))
            ^^^^^^^^^^^^^^^
  Error: Repository "foo" is defined multiple times:
  - dune-workspace:4
  - dune-workspace:7
  [1]

Attempting to redefine a default repository (upstream, overlay) is not allowed:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (repository
  >  (name upstream)
  >  (url "git+file//my-upstream"))
  > EOF

  $ dune build
  File "dune-workspace", line 4, characters 6-29:
  4 |  (url "git+file//my-upstream"))
            ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Repository "upstream" is a default repository and cannot be redefined.
  [1]

Defining the same default repository twice also produces the same error:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (repository
  >  (name upstream)
  >  (url "git+file//my-upstream"))
  > (repository
  >  (name upstream)
  >  (url "git+file//my-upstream-2"))
  > EOF

  $ dune build
  File "dune-workspace", line 4, characters 6-29:
  4 |  (url "git+file//my-upstream"))
            ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Repository "upstream" is a default repository and cannot be redefined.
  [1]

