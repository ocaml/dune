The (inherit ...) field of a tool group, an alternative to (lock_dir ...). It
names a context and optionally restricts which of its packages are shared.

A group inheriting from the default context:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (inherit (context default)))
  > EOF
  $ dune build

A group inheriting from a named context and sharing only some packages:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (context (default (name other)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (inherit (context other) (shared_packages ocaml)))
  > EOF
  $ dune build

The inherited context must exist:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (name dev)
  >  (tools ocaml-lsp-server)
  >  (inherit (context nosuch)))
  > EOF
  $ dune build
  File "dune-workspace", line 6, characters 19-25:
  6 |  (inherit (context nosuch)))
                         ^^^^^^
  Error: Context "nosuch" is not defined.
  [1]

An opam context has no lock directory to inherit:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (context (opam (switch foo)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (inherit (context foo)))
  > EOF
  $ dune build
  File "dune-workspace", line 6, characters 19-22:
  6 |  (inherit (context foo)))
                         ^^^
  Error: Context "foo" is an opam context and has no lock directory to inherit.
  [1]

inherit and lock_dir are mutually exclusive:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (name dev)
  >  (tools ocaml-lsp-server)
  >  (inherit (context default))
  >  (lock_dir (constraints (ocaml (= 5.3.0)))))
  > EOF
  $ dune build
  File "dune-workspace", lines 3-7, characters 0-123:
  3 | (tool_group
  4 |  (name dev)
  5 |  (tools ocaml-lsp-server)
  6 |  (inherit (context default))
  7 |  (lock_dir (constraints (ocaml (= 5.3.0)))))
  Error: fields "lock_dir" and "inherit" are mutually exclusive.
  [1]

shared_packages only exists under inherit, not under lock_dir:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (shared_packages ocaml)))
  > EOF
  $ dune build
  File "dune-workspace", line 5, characters 12-27:
  5 |  (lock_dir (shared_packages ocaml)))
                  ^^^^^^^^^^^^^^^
  Error: Unknown field "shared_packages"
  [1]

The same tool may be declared once per context, and a group without inherit
may coexist with groups that inherit:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (context default)
  > (context (default (name other)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (inherit (context default)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (inherit (context other)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir))
  > EOF
  $ dune build

But not twice for the same context:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (inherit (context default)))
  > (tool_group
  >  (name dev)
  >  (tools ocaml-lsp-server utop)
  >  (inherit (context default)))
  > EOF
  $ dune build
  File "dune-workspace", line 8, characters 8-24:
  8 |  (tools ocaml-lsp-server utop)
              ^^^^^^^^^^^^^^^^
  Error: Tool "ocaml-lsp-server" is declared multiple times for context
  "default":
  - dune-workspace:4
  - dune-workspace:8
  Hint: A tool may be declared once per inherited context, and once among
  groups that do not inherit a context.
  [1]
