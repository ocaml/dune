Tests for the (tool_group ...) stanza in dune-workspace.

A group declaring a single tool:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (tools ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build

A named group with several tools, version constraints, and lock_dir
fields:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (name dev)
  >  (tools ocamlformat (utop (>= 2.15.0)))
  >  (lock_dir
  >   (repositories upstream)
  >   (constraints (ocaml (= 5.3.0)))))
  > EOF
  $ dune build

The stanza is unreleased and unavailable unless it's explicitly enabled with
(using unlreleased):

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
  Error: 'tool_group' is available only when unreleased is enabled in the
  dune-project or workspace file. You must enable it using (using unreleased
  0.1) in the file.
  Note however that unreleased is experimental and might change without notice
  in the future.
  [1]

Declaring the same tool in two groups is an error:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (tools ocamlformat)
  >  (lock_dir))
  > (tool_group
  >  (tools utop ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 7, characters 13-24:
  7 |  (tools utop ocamlformat)
                   ^^^^^^^^^^^
  Error: Tool "ocamlformat" is declared multiple times:
  - dune-workspace:4
  - dune-workspace:7
  Hint: A tool may be declared once per inherited context, and once among
  groups that do not inherit a context.
  [1]

So is declaring it twice within one group:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (tools ocamlformat ocamlformat)
  >  (lock_dir))
  > EOF
  $ dune build
  File "dune-workspace", line 4, characters 20-31:
  4 |  (tools ocamlformat ocamlformat)
                          ^^^^^^^^^^^
  Error: Tool "ocamlformat" is declared multiple times:
  - dune-workspace:4
  - dune-workspace:4
  Hint: A tool may be declared once per inherited context, and once among
  groups that do not inherit a context.
  [1]

Two groups may not share a name:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
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
  File "dune-workspace", line 8, characters 7-10:
  8 |  (name dev)
             ^^^
  Error: Tool group "dev" is declared multiple times:
  - dune-workspace:4
  - dune-workspace:8
  [1]
