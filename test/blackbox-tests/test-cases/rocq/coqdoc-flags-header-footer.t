Testing the rocqdoc_header and rocqdoc_footer field of the env stanza.

  $ make_rocq_project 3.21 0.11

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (rocq
  >    (rocqdoc_header header.html)
  >    (rocqdoc_footer footer.html))))
  > (rocq.theory
  >  (name a))
  > EOF
  $ cat > foo.v <<EOF
  > Definition a := 42.
  > EOF
  $ cat > header.html <<EOF
  > header
  > EOF
  $ cat > footer.html <<EOF
  > footer
  > EOF

  $ dune build @doc

  $ dune trace cat | jq_dune -c 'coqdocFlags'
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "a"
  "--toc"
  "--with-header"
  "header.html"
  "--with-footer"
  "footer.html"
  "--html"
  "-d"
  "a.html"
  "foo.v"

  $ dune build @doc-latex

  $ dune trace cat | jq_dune -c 'coqdocFlags'
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "a"
  "--toc"
  "--latex"
  "-d"
  "a.tex"
  "foo.v"
