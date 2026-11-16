With (odoc auto), the default from lang 3.26 with the unreleased extension,
the @doc aliases skip the packages that have neither a library nor a
documentation stanza.

  $ cat > dune-project << EOF
  > (lang dune 3.26)
  > (using unreleased 0.1)
  > (package (name foo))
  > (package (name bar))
  > EOF
  $ mkdir foo bar
  $ cat > foo/dune << EOF
  > (executable (public_name foo) (package foo))
  > EOF
  $ touch foo/foo.ml
  $ cat > bar/dune << EOF
  > (library (public_name bar))
  > EOF
  $ touch bar/bar.ml

  $ dune build @doc
  $ find _build/default/_doc/_html -name index.html | sort
  _build/default/_doc/_html/bar/Bar/index.html
  _build/default/_doc/_html/bar/index.html
  _build/default/_doc/_html/foo/index.html
  _build/default/_doc/_html/index.html
  $ grep -o 'href="[a-z]*/index.html"' _build/default/_doc/_html/index.html
  href="bar/index.html"
  href="foo/index.html"

  $ dune build @doc-json
  $ cat _build/default/_doc/_html/index.html.json
  {"packages":[{"name":"bar","version":null,"link":"bar/index.html"},{"name":"foo","version":null,"link":"foo/index.html"}]}

  $ dune build @doc-markdown
  $ find _build/default/_doc/_markdown -name index.md | sort
  _build/default/_doc/_markdown/bar/index.md
  _build/default/_doc/_markdown/foo/index.md
  _build/default/_doc/_markdown/index.md
  $ grep -o '([a-z]*/index.md)' _build/default/_doc/_markdown/index.md
  (bar/index.md)
  (foo/index.md)

A package with only a documentation stanza, or only a private library, is
documented:

  $ mkdir more && cd more
  $ cat > dune-project << EOF
  > (lang dune 3.26)
  > (using unreleased 0.1)
  > (package (name mld))
  > (package (name priv))
  > EOF
  $ mkdir mld priv
  $ cat > mld/dune << EOF
  > (documentation (package mld))
  > EOF
  $ cat > mld/index.mld << EOF
  > {0 Mld}
  > EOF
  $ cat > priv/dune << EOF
  > (library (name priv) (package priv))
  > EOF
  $ touch priv/priv.ml
  $ dune build @doc
  $ grep -o 'href="[a-z]*/index.html"' _build/default/_doc/_html/index.html
  href="mld/index.html"
  href="priv/index.html"
  $ cd ..

(odoc auto) only applies to packages defined in dune-project. Those defined
by an opam file keep (odoc true):

  $ mkdir opam && cd opam
  $ cat > dune-project << EOF
  > (lang dune 3.26)
  > (using unreleased 0.1)
  > EOF
  $ touch foo.opam
  $ cat > dune << EOF
  > (executable (public_name foo))
  > EOF
  $ touch foo.ml
  $ dune build @doc
  $ grep -o 'href="[a-z]*/index.html"' _build/default/_doc/_html/index.html
  href="foo/index.html"
  $ cd ..
