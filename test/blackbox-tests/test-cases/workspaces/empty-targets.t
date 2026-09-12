An explicitly empty workspace target list is accepted. The native context is
still available.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context (default (targets)))
  > EOF

  $ dune build

  $ dune describe contexts
  default
