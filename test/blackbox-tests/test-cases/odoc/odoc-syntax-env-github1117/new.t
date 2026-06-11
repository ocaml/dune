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


  $ html=_build/default/_doc_new/html/docs/local/l/L/index.html
  $ dune build "$html"
  $ odoc_detect_syntax "$html"
  it is ocaml

  $ ODOC_SYNTAX=re dune build "$html"
  $ odoc_detect_syntax "$html"
  it is reason
