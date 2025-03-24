Fallback rules must either have:

* all their targets in the source
* no targets in the source

Here we me makes sure that having some targets in the source and some not is
impossible.

  $ cat>dune-project <<EOF
  > (lang dune 3.10)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (action
  >   (progn
  >    (with-stdout-to x (echo ""))
  >    (with-stdout-to y (echo ""))))
  >  (mode fallback)
  >  (targets x y))
  > EOF

  $ build() { dune build x y; }

  $ build

  $ touch x

  $ build
  File "dune", lines 1-7, characters 0-122:
  1 | (rule
  2 |  (action
  3 |   (progn
  4 |    (with-stdout-to x (echo ""))
  5 |    (with-stdout-to y (echo ""))))
  6 |  (mode fallback)
  7 |  (targets x y))
  Error: Some of the targets of this fallback rule are present in the source
  tree, and some are not. This is not allowed. Either none of the targets must
  be present in the source tree, either they must all be.
  
  The following targets are present:
  - x
  
  The following targets are not:
  - y
  [1]
