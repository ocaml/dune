Testing the rocqdoc_header and rocqdoc_footer field of the env stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

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

  $ dune trace cat | jq -c 'include "dune"; select(.args.process_args.[0] == "doc") | .args.process_args | .[] | sub(".*/coq/"; "coq/")'
  "doc"
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

  $ dune trace cat | jq -c 'include "dune"; processes | select(.args.process_args.[0] == "doc") | .args.process_args | .[] | sub(".*/coq/"; "coq/")'
  "doc"
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
