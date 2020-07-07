  $ [ -e dune-project ] || echo File does not exist
  File does not exist
  $ mkdir src
  $ echo '(rule (alias runtest) (action (progn)))' >  src/dune
  $ dune build 2>&1 | sed "s/(lang dune .*)/(lang dune <version>)/"
  Info: Creating file dune-project with this contents:
  | (lang dune <version>)
  $ cat dune-project | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)

Test that using menhir automatically update the dune-project file

  $ echo '(library (name x)) (menhir (modules x))' >> src/dune
  $ dune build @install 2>&1 | sed "s/(using menhir .*)/(using menhir <version>)/"
  Info: Appending this line to dune-project: (using menhir <version>)
  $ cat dune-project | sed "s/(lang dune .*)/(lang dune <version>)/" | sed "s/(using menhir .*)/(using menhir <version>)/"
  (lang dune <version>)
  (using menhir <version>)
