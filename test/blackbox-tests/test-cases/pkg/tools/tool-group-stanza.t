Tests for the (tool_group ...) stanza in dune-workspace.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > EOF

A group declaring a single tool:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build

A named group with several tools, version constraints, and lock_dir
fields:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (name dev)
  >  (tools ocamlformat (utop (>= 2.15.0)))
  >  (lock_dir
  >   (repositories upstream)
  >   (constraints (ocaml (= 5.3.0)))))
  > EOF
  $ dune build

The stanza is versioned and unavailable in older versions of the dune
language:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.24)
  > (tool_group
  >  (tools ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", lines 2-4, characters 0-45:
  2 | (tool_group
  3 |  (tools ocamlformat)
  4 |  (lock_dir))
  Error: 'tool_group' is only available since version 3.25 of the dune
  language. Please update your dune-project file to have (lang dune 3.25).
  [1]

Declaring the same tool in two groups is an error:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocamlformat)
  >  (lock_dir))
  > (tool_group
  >  (tools utop ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 6, characters 13-24:
  6 |  (tools utop ocamlformat)
                   ^^^^^^^^^^^
  Error: Tool "ocamlformat" is defined multiple times:
  - dune-workspace:3
  - dune-workspace:6
  [1]

So is declaring it twice within one group:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocamlformat ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 3, characters 20-31:
  3 |  (tools ocamlformat ocamlformat)
                          ^^^^^^^^^^^
  Error: Tool "ocamlformat" is defined multiple times:
  - dune-workspace:3
  - dune-workspace:3
  [1]

Two groups may not share a name:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (name dev)
  >  (tools ocamlformat)
  >  (lock_dir))
  > (tool_group
  >  (name dev)
  >  (tools utop)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 7, characters 7-10:
  7 |  (name dev)
             ^^^
  Error: Tool group "dev" is defined multiple times:
  - dune-workspace:3
  - dune-workspace:7
  [1]
