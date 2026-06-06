Testing the coqdoc flags field of the env stanza.

  $ make_rocq_project 3.21 0.11

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (rocq
  >    (rocqdoc_flags :standard -toc-depth 2))))
  > (rocq.theory
  >  (name a))
  > EOF
  $ cat > foo.v <<EOF
  > Definition a := 42.
  > EOF

  $ dune build @doc

  $ dune trace cat | jq_dune 'coqdocFlags'
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "a"
  "--toc"
  "-toc-depth"
  "2"
  "--html"
  "-d"
  "a.html"
  "foo.v"
