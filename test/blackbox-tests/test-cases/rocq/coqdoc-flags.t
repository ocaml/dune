Testing the coqdoc flags field of the env stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

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

  $ dune trace cat | jq 'include "dune"; processes | select(.args.process_args.[0] == "doc") | .args.process_args | .[] | sub(".*/coq/"; "coq/")'
  "doc"
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
