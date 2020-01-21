Reproduction case for #1529: using an extension when no dune-project
file is present.

  $ dune build @install 2>&1 | sed "s/(lang dune .*)/(lang dune <version>)/" | sed "s/(using menhir .*)/(using menhir <version>)/"
  Info: Creating file dune-project with this contents:
  | (lang dune <version>)
  Info: Appending this line to dune-project: (using menhir <version>)
