We don't need to run ocamldep on ther alias module

  $ make_dune_project 3.19

  $ cat >dune <<EOF
  > (library
  >  (name foo))
  > EOF

  $ touch bar.ml

  $ dune build foo.cma

  $ find _build -iname "*.d" -o -iname "*.all-deps" | sort
  _build/default/.foo.objs/foo__Bar.impl.all-deps
  _build/default/.foo.objs/foo__Bar.impl.d
