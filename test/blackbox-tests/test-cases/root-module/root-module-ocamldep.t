Demonstrate if we're running ocamldep for the root module. There should be no
need to do so since this module cannot depend on any other module in the same
compilation unit.

  $ make_dune_project 3.20

  $ mkdir lib/
  $ cat >lib/dune <<EOF
  > (library
  >  (name bar))
  > EOF
  $ touch lib/bar.ml

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (libraries bar)
  >  (root_module root))
  > EOF

  $ dune build foo.cma

  $ find _build/default/.foo.objs | grep -i root | sort -u
  _build/default/.foo.objs/byte/foo__Root.cmi
  _build/default/.foo.objs/byte/foo__Root.cmo
  _build/default/.foo.objs/byte/foo__Root.cmt

Not only is running ocamldep wasteful, but it can also lead to cycles:

  $ cat >bar.ml <<EOF
  > module X = Root.Bar
  > EOF

  $ dune build foo.cma
