The rules that call odoc know that it is going to read the ODOC_SYNTAX
variable, and can rebuild as needed.

  $ cat > dune-project << EOF
  > (lang dune 1.1)
  > (package (name l))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name l))
  > EOF

  $ cat > l.ml << EOF
  > module type X = sig end
  > EOF


  $ dune build @doc-new
  $ odoc_detect_syntax _build/default/_doc_new/html/docs/local/l/L/index.html
  it is ocaml

  $ ODOC_SYNTAX=re dune build @doc-new
  $ odoc_detect_syntax _build/default/_doc_new/html/docs/local/l/L/index.html
  it is reason
