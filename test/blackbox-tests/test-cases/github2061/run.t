Reproduction case for #2061. Make sure dune behaves sensibly when a rule
in standard mode produces multiple files and only a strict subset of them is in
the source tree.

Dune < 1.10
-----------

  $ echo '(lang dune 1.9)' > dune-project

  $ dune build a
  File "dune", line 1, characters 0-95:
  1 | (rule
  2 |  (progn
  3 |   (with-stdout-to a (echo "GENERATED"))
  4 |   (with-stdout-to b (echo "GENERATED"))))
  Warning: The following files are both generated by a rule and are present in
  the source tree:
  - a
  Because your project was written for dune 1.9, I am closing my eyes on this
  and I am overwriting the source files with the generated one. However, you
  should really delete these files from your source tree. I will no longer
  accept this once you upgrade your project to dune >= 1.10.
  $ cat _build/default/a
  GENERATED

Building b shouldn't erase a:

  $ dune build b
  File "dune", line 1, characters 0-95:
  1 | (rule
  2 |  (progn
  3 |   (with-stdout-to a (echo "GENERATED"))
  4 |   (with-stdout-to b (echo "GENERATED"))))
  Warning: The following files are both generated by a rule and are present in
  the source tree:
  - a
  Because your project was written for dune 1.9, I am closing my eyes on this
  and I am overwriting the source files with the generated one. However, you
  should really delete these files from your source tree. I will no longer
  accept this once you upgrade your project to dune >= 1.10.
  $ cat _build/default/a
  GENERATED

Dune >= 1.10
------------

  $ echo '(lang dune 1.10)' > dune-project
  $ dune build a
  Multiple rules generated for _build/default/a:
  - file present in source tree
  - dune:1
  Hint: rm -f a
  [1]
