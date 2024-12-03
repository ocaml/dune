This test checks that if a library is declared with `(no_dynlink)`, then the
corresponding `.cmxs` file is *not* built.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

First we check the behaviour when `(no_dynlink)` is not present.

  $ cat >dune <<EOF
  > (library
  >  (name mylib))
  > EOF

  $ touch a.ml

  $ dune build _build/default/mylib.cmxs

Now with `(no_dynlink)`.

  $ cat >dune <<EOF
  > (library
  >  (name mylib)
  >  (no_dynlink))
  > EOF

  $ dune clean

  $ dune build _build/default/mylib.cmxs
  Error: Don't know how to build _build/default/mylib.cmxs
  Hint: did you mean _build/default/mylib.cma or _build/default/mylib.cmxa?
  [1]
