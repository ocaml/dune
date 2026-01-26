Testing the coqdoc_header and coqdoc_footer field of the env stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using coq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (rule (with-stdout-to header.%{coq:version}.html (echo "HEADER")))
  > (rule (with-stdout-to footer.html (echo "FOOTER")))
  > (env
  >  (_
  >   (coq
  >    (coqdoc_header header.%{coq:version}.html)
  >    (coqdoc_footer footer.html))))
  > (coq.theory
  >  (name a))
  > EOF
  $ cat > foo.v <<EOF
  > Definition a := 42.
  > EOF

  $ dune build @doc
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ find _build/default/a.html -type f -name '*.html' | sort | xargs grep HEADER
  _build/default/a.html/a.foo.html:HEADER
  _build/default/a.html/index.html:HEADER
  _build/default/a.html/toc.html:HEADER
  $ find _build/default/a.html -type f -name '*.html' | sort | xargs grep FOOTER
  _build/default/a.html/a.foo.html:FOOTER
  _build/default/a.html/index.html:FOOTER
  _build/default/a.html/toc.html:FOOTER
