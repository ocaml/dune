Detect include loops in files included in the install stanza

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name hello))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (public_name hello))
  > 
  > (install
  >  (files (include foo.sexp))
  >  (section share))
  > EOF

  $ cat >hello.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

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
