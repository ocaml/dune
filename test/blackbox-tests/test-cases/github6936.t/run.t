Reproduction of https://github.com/savonet/ocaml-taglib/issues/10

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF

  $ touch taglib.opam

Build fine
  $ dune build -p taglib -j 1
