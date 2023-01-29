Found while creating a reproduction of https://github.com/savonet/ocaml-taglib/issues/10

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF

  $ touch taglib.opam

Build fine
  $ dune build -p taglib -j 1

Remove depency on config.h
  $ sed -i -e '/config.h/d' dune
  $ dune build -p taglib -j 1

Don't create config.h and it still build 😯
  $ sed -i -e '/config.h/d' config/discover.ml
  $ { dune build -p taglib -j 1 2>&1; echo $?; } > result_before

We should have the same result
  $ dune clean
  $ { dune build -p taglib -j 1 2>&1; echo $?; } > result_after

We should have the same failure
  $ diff result_before result_after
