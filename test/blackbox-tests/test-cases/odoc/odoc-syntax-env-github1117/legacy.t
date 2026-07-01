The rules that call odoc know that it is going to read the ODOC_SYNTAX
variable, and can rebuild as needed.

  $ make_dune_project_with_package 1.1 l

  $ cat > dune << EOF
  > (library
  >  (public_name l))
  > EOF

  $ cat > l.ml << EOF
  > module type X = sig end
  > EOF


  $ dune build @doc
  $ odoc_detect_syntax _build/default/_doc/_html/l/L/index.html
  it is ocaml

  $ ODOC_SYNTAX=re dune build @doc
  $ odoc_detect_syntax _build/default/_doc/_html/l/L/index.html
  it is reason
