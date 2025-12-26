dune trace cat can be used to view the trace:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ dune build
  $ dune trace cat | jq -c '.[] | keys'
  ["args","cat","name","ph","pid","tid","ts"]
  ["args","cat","name","ph","pid","tid","ts"]
  ["args","cat","name","ph","pid","tid","ts"]
  ["args","cat","name","ph","pid","tid","ts"]
  ["args","cat","name","ph","pid","tid","ts"]
  ["args","cat","name","ph","pid","tid","ts"]
  ["args","cat","dur","name","ph","pid","tid","ts"]
  ["args","cat","name","ph","pid","tid","ts"]
  ["args","cat","dur","name","ph","pid","tid","ts"]
  ["args","cat","dur","name","ph","pid","tid","ts"]
  ["cat","name","ph","pid","tid","ts"]
