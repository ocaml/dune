# Sample dune-coq project

This is a simple dune build which has 2 projects:

1. `example-ocaml` - simple pure OCaml library
2. `example-coq` - simple OCaml library extracted from Coq

In addition to Coq to OCaml extraction, it also demonstrates a
workaround for long-standing Coq [issue](https://github.com/coq/coq/issues/6614) and until it is fixed
could be used as a template for dune projects using Coq extraction.
