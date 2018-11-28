Reproduction case for #1560: by default, `dune` files inside the .git
directory should be ignored

  $ echo '(lang dune 1.6)' > dune-project
  $ mkdir .git
  $ echo 'invalid dune file' > .git/dune

The invalid `dune` fine should be ignored:

  $ dune build

Testing in presence of an empty `dune` file:

  $ echo > dune
  $ dune build

Testing in presence of an `ignored_subdirs` stanza:

  $ echo '(ignored_subdirs (blah))' > dune
  $ dune build
