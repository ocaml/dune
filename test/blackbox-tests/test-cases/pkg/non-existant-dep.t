A package depending on a package that doesn't exist.
The solver should give a more sane error message.

  $ cat > dune-project << EOF
  > (lang dune 3.15)
  > (name abc)
  > (package
  > (name abc)
  > (synopsis "A short synopsis")
  > (description "A longer description")
  > (depends
  >   ocaml
  >   dune
  >   base
  >   re
  >   unix
  >   foobar
  >   core
  >   core_unix
  >   ocamlformat
  >   ocamlformat-lib)
  > (tags
  >  (topics "to describe" your project)))
  > EOF

  $ dune pkg lock
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  The following packages couldn't be found: - foobar - unix
  
  [1]
