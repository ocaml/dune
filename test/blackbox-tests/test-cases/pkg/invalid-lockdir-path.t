Test that we get the expected error when a context specifies an invalid lockdir path

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (lock foo)))

  $ dune build
  File "dune-workspace", line 4, characters 8-11:
  4 |   (lock foo)))
              ^^^
  Error: lockdir path foo does not have required extension ".lock"
  [1]
