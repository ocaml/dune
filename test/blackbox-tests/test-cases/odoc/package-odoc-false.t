The (odoc false) field of the package stanza marks a package as having no
documentation. The @doc aliases should not build documentation for it.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using unreleased 0.1)
  > (package (name foo) (odoc false))
  > (package (name bar))
  > EOF

  $ mkdir foo bar
  $ cat > foo/dune << EOF
  > (library (public_name foo))
  > EOF
  $ cat > foo/foo.mli << EOF
  > (** Foo's type *)
  > type t = int
  > EOF
  $ touch foo/foo.ml

The documentation of bar refers to a type from foo:

  $ cat > bar/dune << EOF
  > (library (public_name bar) (libraries foo))
  > EOF
  $ cat > bar/bar.mli << EOF
  > (** A value of type {!Foo.t} *)
  > val x : Foo.t
  > EOF
  $ cat > bar/bar.ml << EOF
  > let x = 0
  > EOF

  $ list_indexes () {
  >   find _build/default/_doc/$1 -name "index.$2" | sort
  > }

HTML documentation:

  $ dune build @doc
  $ list_indexes _html html
  _build/default/_doc/_html/bar/Bar/index.html
  _build/default/_doc/_html/bar/index.html
  _build/default/_doc/_html/foo/Foo/index.html
  _build/default/_doc/_html/foo/index.html
  _build/default/_doc/_html/index.html
  $ grep -o 'href="[a-z]*/index.html"' _build/default/_doc/_html/index.html
  href="bar/index.html"
  href="foo/index.html"

The documentation of bar links to the page of Foo.t:

  $ grep -o 'href="[^"]*foo[^"]*"' _build/default/_doc/_html/bar/Bar/index.html | sort -u
  href="../../foo/Foo/index.html#type-t"

JSON documentation:

  $ dune build @doc-json
  $ cat _build/default/_doc/_html/index.html.json
  {"packages":[{"name":"bar","version":null,"link":"bar/index.html"},{"name":"foo","version":null,"link":"foo/index.html"}]}

Markdown documentation:

  $ dune build @doc-markdown
  $ list_indexes _markdown md
  _build/default/_doc/_markdown/bar/index.md
  _build/default/_doc/_markdown/foo/index.md
  _build/default/_doc/_markdown/index.md
  $ grep -o '([a-z]*/index.md)' _build/default/_doc/_markdown/index.md
  (bar/index.md)
  (foo/index.md)
