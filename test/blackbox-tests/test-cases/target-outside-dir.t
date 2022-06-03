Generate rules in sub directories. There's two ways targets are specified:

* Inferred from the action
* Specified by the user

The restriction on generating targets should be the same on both.

  $ cat >dune-project <<EOF
  > (lang dune 3.2)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to foo/bar.ml (echo "let foo = 42;;")))
  > EOF

  $ dune build foo/bar.ml

  $ cat >dune <<EOF
  > (rule
  >  (targets foo/bar.ml)
  >  (action (with-stdout-to foo/bar.ml (echo "let foo = 42;;"))))
  > EOF

  $ dune build foo/bar.ml
  File "dune", line 2, characters 10-20:
  2 |  (targets foo/bar.ml)
                ^^^^^^^^^^
  Error: "foo/bar.ml" does not denote a file in the current directory.
  [1]
