Reproduce #8281

When a secondary .opam file has an invalid package name, dune should not crash
when building @doc.

  $ make_dune_project 2.4
  $ touch x.opam x.y.opam

  $ mkdir x && cd x
  $ cat >dune <<EOF
  > (library
  >  (public_name x))
  > EOF

  $ mkdir y && cd y
  $ cat >dune <<EOF
  > (library
  >  (public_name x.y)
  >  (name x_y))
  > EOF
  $ cd ..

  $ cd ..

  $ dune build @doc
