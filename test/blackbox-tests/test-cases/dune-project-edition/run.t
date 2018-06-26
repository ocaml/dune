  $ cat dune-project
  cat: dune-project: No such file or directory
  [1]
  $ mkdir src
  $ echo '(alias (name runtest) (action (progn)))' >  src/dune
  $ dune build
  Info: creating file dune-project with this contents: (lang dune 1.0)
  $ cat dune-project
  (lang dune 1.0)

Test that using menhir automatically update the dune-project file

  $ echo '(library (name x)) (menhir (modules x))' >> src/dune
  $ dune build
  Info: appending this line to dune-project: (using menhir 1.0)
  $ cat dune-project
  (lang dune 1.0)
  (using menhir 1.0)
