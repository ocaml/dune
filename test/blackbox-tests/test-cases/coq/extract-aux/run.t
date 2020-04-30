A simple repository with Coq files and an extraction to OCaml.
The Coq files are separated in several modules and are doing some proofs in order for .aux files to be generated (in contrary to the very simple [extract] test which only test one file).
The reason for this test is that in some cases, it seems that these auxiliary files are causing problems.

  $ dune build @all

