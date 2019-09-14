Check that local variant implementations are correctly exported in the list of
known_implementations implementations when using -p

  $ cd project && dune build -p vlibfoo
  File "implfoo/dune", line 1, characters 0-94:
  1 | (library
  2 |  (name implfoo)
  3 |  (public_name implfoo)
  4 |  (implements vlibfoo)
  5 |  (variant somevariant)
  6 | )
  Error: Library "implfoo" not found.
  Hint: try: dune external-lib-deps --missing -p vlibfoo @install
  [1]

  $ cat project/_build/install/default/lib/vlibfoo/dune-package
  cat: project/_build/install/default/lib/vlibfoo/dune-package: No such file or directory
  [1]

Also check that the implementation correctly builds while using -p when part of the same project

  $ cp -r project/_build/ opam

  $ cd project && env OCAMLPATH=../opam/install/default/lib dune build -p implfoo
  File "implfoo/dune", line 4, characters 13-20:
  4 |  (implements vlibfoo)
                   ^^^^^^^
  Error: Library "vlibfoo" is not virtual. It cannot be implemented by
  "implfoo".
  [1]

And fail if it's not part of the same project.

  $ cd project-2 && env OCAMLPATH=../opam/install/default/lib dune build -p impl2foo
  File "impl2foo/dune", line 4, characters 13-20:
  4 |  (implements vlibfoo)
                   ^^^^^^^
  Error: Library "vlibfoo" is not virtual. It cannot be implemented by
  "impl2foo".
  [1]
