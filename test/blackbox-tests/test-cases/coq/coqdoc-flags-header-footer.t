Testing the coqdoc_header and coqdoc_footer field of the env stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (using coq 0.10)
  > EOF

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (coq
  >    (coqdoc_header header.html)
  >    (coqdoc_footer footer.html))))
  > (coq.theory
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

  $ tail _build/log -n 1 | ./scrub_coq_args.sh | sed 's/.*coq/coq/'
  coqdoc
  coq/theories Coq
  -R . a --toc --with-header header.html --with-footer footer.html --html -d a.html
  foo.v

  $ dune build @doc-latex

  $ tail _build/log -n 1 | ./scrub_coq_args.sh | sed 's/.*coq/coq/'
  coqdoc
  coq/theories Coq
  -R . a --toc --latex -d a.tex
  foo.v
