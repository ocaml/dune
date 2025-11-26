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

  $ tail _build/log -n 1 | ./scrub_coq_args.sh | sed 's/.*rocq/rocq/'
  rocq doc
  -R coq/theories Corelib
  -R . a --toc --with-header header.html --with-footer footer.html --html -d a.html
  foo.v

  $ dune build @doc-latex

  $ tail _build/log -n 1 | ./scrub_coq_args.sh | sed 's/.*rocq/rocq/'
  rocq doc
  -R coq/theories Corelib
  -R . a --toc --latex -d a.tex
  foo.v
