Demonstrate OCaml snippet not shown for files with directives

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ mkdir a b
  $ cat > a/original.ml <<EOF
  > let x = Some "hello"
  > let Some _y = x
  > EOF

  $ cp a/original.ml b/copied.ml
  $ cat > b/dune <<EOF
  > (copy_files#
  >   ../a/original.ml)
  > 
  > (library
  >  (name b)
  >  (modes byte)
  >  (modules original copied)
  >  (flags -w +a-70))
  > EOF

  $ dune build
  File "b/copied.ml", line 2, characters 4-11:
  2 | let Some _y = x
          ^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  None
  File "a/original.ml", line 2, characters 4-11:
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  None
