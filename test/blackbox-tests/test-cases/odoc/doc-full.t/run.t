Test @doc-full builds documentation for installed packages

Setup a simple project:

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package (name mylib))
  > EOF

  $ cat > mylib.opam <<EOF
  > opam-version: "2.0"
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.ml <<EOF
  > (** My library *)
  > let hello () = "Hello, world!"
  > EOF

Build documentation with @doc-full:

  $ dune build @doc-full

Check that local package HTML was built in _html_full:

  $ ls _build/default/_doc/_html_full/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html_full/mylib/mylib/Mylib/index.html

Check that the root index exists:

  $ ls _build/default/_doc/_html_full/index.html
  _build/default/_doc/_html_full/index.html

Both _html (local-only) and _html_full directories should be independent:

  $ dune build @doc

  $ ls _build/default/_doc/_html/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html/mylib/mylib/Mylib/index.html
