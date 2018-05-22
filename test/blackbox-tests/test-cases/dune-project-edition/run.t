  $ cat dune-project
  cat: dune-project: No such file or directory
  [1]
  $ mkdir src
  $ echo '(alias ((name runtest) (action (progn))))' >  src/dune
  $ dune build
  Info: creating file dune-project with this contents: (lang dune 1.0)
  $ cat dune-project
  (lang dune 1.0)
