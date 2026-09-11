The (inherit ...) and (share ...) fields of a tool group's lock_dir block.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > EOF

A group inheriting from the default context:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit default)))
  > EOF
  $ dune build

A group inheriting from a named context and sharing only some packages:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (context (default (name other)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit other) (share ocaml)))
  > EOF
  $ dune build

The inherited context must exist:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit nosuch)))
  > EOF
  $ dune build
  File "dune-workspace", line 4, characters 20-26:
  4 |  (lock_dir (inherit nosuch)))
                          ^^^^^^
  Error: Context "nosuch" is not defined.
  [1]

An opam context has no lock directory to inherit:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (context (opam (switch foo)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit foo)))
  > EOF
  $ dune build
  File "dune-workspace", line 5, characters 20-23:
  5 |  (lock_dir (inherit foo)))
                          ^^^
  Error: Context "foo" is an opam context and has no lock directory to inherit.
  [1]

No other lock_dir field may be combined with inherit for now:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit default) (constraints (ocaml (= 5.3.0)))))
  > EOF
  $ dune build
  File "dune-workspace", line 4, characters 29-60:
  4 |  (lock_dir (inherit default) (constraints (ocaml (= 5.3.0)))))
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This field cannot be combined with "inherit" yet.
  [1]

share requires inherit:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (share ocaml)))
  > EOF
  $ dune build
  File "dune-workspace", line 4, characters 18-23:
  4 |  (lock_dir (share ocaml)))
                        ^^^^^
  Error: "share" requires "inherit".
  [1]

The same tool may be declared once per context, and a group without inherit
may coexist with groups that inherit:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (context default)
  > (context (default (name other)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit default)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit other)))
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir))
  > EOF
  $ dune build

But not twice for the same context:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (tool_group
  >  (tools ocaml-lsp-server)
  >  (lock_dir (inherit default)))
  > (tool_group
  >  (tools ocaml-lsp-server utop)
  >  (lock_dir (inherit default)))
  > EOF
  $ dune build
  File "dune-workspace", line 6, characters 8-24:
  6 |  (tools ocaml-lsp-server utop)
              ^^^^^^^^^^^^^^^^
  Error: Tool "ocaml-lsp-server" is defined multiple times for context
  "default":
  - dune-workspace:3
  - dune-workspace:6
  [1]
