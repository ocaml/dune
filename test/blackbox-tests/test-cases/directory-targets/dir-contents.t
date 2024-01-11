Directory targets and ocaml/coq/etc sources

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ cat >produce.sh <<EOF
  > mkdir sources
  > cd sources
  > echo "print_endline Foo.bar;;" > main.ml
  > echo "let bar = "hello world" > main.ml
  > EOF

  $ chmod +x produce.sh

  $ cat >dune <<EOF
  > (rule
  >  (targets (dir sources))
  >  (action (run ./produce.sh)))
  > (include_subdirs unqualified)
  > (executable (name main))
  > EOF

  $ dune exec ./main.exe
  File "dune", line 5, characters 18-22:
  5 | (executable (name main))
                        ^^^^
  Error: Module "Main" doesn't exist.
  [1]

