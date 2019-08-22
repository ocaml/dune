Test the `forbidden_libraries` feature

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name a) (modules))
  > (library (name b) (modules) (libraries a))
  > (library (name c) (modules) (libraries b))
  > (executable
  >  (name main)
  >  (libraries c)
  >  (forbidden_libraries a))
  > EOF

  $ touch main.ml

  $ dune build main.exe
  File "dune", line 7, characters 22-23:
  7 |  (forbidden_libraries a))
                            ^
  Error: Library "a" was pulled in.
  -> required by library "b" in _build/default
  -> required by library "c" in _build/default
  [1]
