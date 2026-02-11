Demonstrate file trace events for saved files:

  $ make_dune_project 3.22

  $ cat >dune <<EOF
  > (rule
  >  (targets trace-file.txt)
  >  (action (write-file trace-file.txt "hello")))
  > EOF

  $ dune build trace-file.txt

  $ dune trace cat | jq '
  >   select(.cat == "action" and .name == "write-file")
  > | .args
  > | select(.file | endswith("trace-file.txt"))
  > '
  {
    "file": "_build/default/trace-file.txt",
    "size": 5
  }
