  $ echo "(lang dune 2.0)" > dune-project
  $ cat > dune <<EOF
  > (library (name foo) (libraries (select foo.ml from (!bar -> f.ml))))
  > EOF
  $ dune build
  File "dune", line 1, characters 60-64:
  1 | (library (name foo) (libraries (select foo.ml from (!bar -> f.ml))))
                                                                  ^^^^
  Error: The format for files in this select branch must be foo.{name}.ml
  [1]
  $ cat > dune <<EOF
  > (library (name foo) (libraries (select foo.ml from (!bar -> foo.mli))))
  > EOF
  $ dune build
  File "dune", line 1, characters 60-67:
  1 | (library (name foo) (libraries (select foo.ml from (!bar -> foo.mli))))
                                                                  ^^^^^^^
  Error: The format for files in this select branch must be foo.{name}.ml
  [1]
