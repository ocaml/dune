Testing the rocqdoc_header and rocqdoc_footer field of the env stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (rule (with-stdout-to header.%{rocq:version}.html (echo "HEADER")))
  > (rule (with-stdout-to footer.html (echo "FOOTER")))
  > (env
  >  (_
  >   (rocq
  >    (rocqdoc_header header.%{rocq:version}.html)
  >    (rocqdoc_footer footer.html))))
  > (rocq.theory
  >  (name a))
  > EOF
  $ cat > foo.v <<EOF
  > Definition a := 42.
  > EOF

  $ dune build @doc
  $ find _build/default/a.html -type f -name '*.html' | sort | xargs grep HEADER
  _build/default/a.html/a.foo.html:HEADER
  _build/default/a.html/index.html:HEADER
  _build/default/a.html/toc.html:HEADER
  $ find _build/default/a.html -type f -name '*.html' | sort | xargs grep FOOTER
  _build/default/a.html/a.foo.html:FOOTER
  _build/default/a.html/index.html:FOOTER
  _build/default/a.html/toc.html:FOOTER
