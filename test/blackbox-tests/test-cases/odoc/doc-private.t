@doc-private skips a private library with no compilation modes and still builds
HTML for the other private library.

  $ make_dune_project 3.13

  $ cat > dune <<EOF
  > (library
  >  (name normal)
  >  (modules normal))
  > (library
  >  (name skipped)
  >  (modules skipped)
  >  (modes))
  > EOF
  $ echo 'let x = 1' > normal.ml
  $ cp normal.ml skipped.ml

  $ dune build @doc-private
  $ find _build/default/_doc/_html -name index.html | LC_ALL=C sort \
  >   | dune_cmd subst '@[0-9a-f]+/' '@<scope>/'
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/normal@<scope>/Normal/index.html
