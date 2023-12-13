Testing the coqdoc flags field of the env stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using coq 0.8)
  > EOF

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (coq
  >    (coqdoc_flags :standard -toc-depth 2))))
  > (coq.theory
  >  (name a))
  > EOF
  $ cat > foo.v <<EOF
  > Definition a := 42.
  > EOF

  $ dune build @doc

  $ tail _build/log -n 1 | ./scrub_coq_args.sh | sed 's/.*coq/coq/' 
  coqdoc
  coq/theories Coq
  -R . a --toc -toc-depth 2 --html -d a.html
  foo.v
