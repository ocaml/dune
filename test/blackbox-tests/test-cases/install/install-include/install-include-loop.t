Detect include loops in files included in the install stanza

  $ make_install_include_project

  $ cat >foo.sexp <<EOF
  > ((include bar.sexp))
  > EOF

  $ cat >bar.sexp <<EOF
  > ((include foo.sexp))
  > EOF

  $ dune build @install
  File "_build/default/bar.sexp", line 1, characters 10-18:
  1 | ((include foo.sexp))
                ^^^^^^^^
  Error: Include loop detected via: _build/default/foo.sexp
  [1]
