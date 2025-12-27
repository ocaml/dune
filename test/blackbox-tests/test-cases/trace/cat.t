dune trace cat can be used to view the trace:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ dune build
  $ dune trace cat | jq -c 'keys'
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","dur","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","dur","name","ts"]
  ["args","cat","dur","name","ts"]
  ["args","cat","name","ts"]
