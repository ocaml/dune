Error cases in the (tool_group ...) stanza itself.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > EOF

The tools and lock_dir fields are required:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 2, characters 0-23:
  2 | (tool_group (lock_dir))
      ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Field "tools" is missing
  [1]

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group (tools ocamlformat))
  > EOF
  $ dune build
  File "dune-workspace", line 2, characters 0-32:
  2 | (tool_group (tools ocamlformat))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Field "lock_dir" is missing
  [1]

A group must declare at least one tool:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 3, characters 1-8:
  3 |  (tools)
       ^^^^^^^
  Error: A tool group must declare at least one tool.
  [1]

A tool must be a valid opam package name:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools foo/bar)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 3, characters 8-15:
  3 |  (tools foo/bar)
              ^^^^^^^
  Error: "foo/bar" is an invalid package dependency.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: foo_bar would be a correct package dependency
  [1]

Unknown fields are rejected:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocamlformat)
  >  (binaries ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 4, characters 2-10:
  4 |  (binaries ocamlformat)
        ^^^^^^^^
  Error: Unknown field "binaries"
  [1]
