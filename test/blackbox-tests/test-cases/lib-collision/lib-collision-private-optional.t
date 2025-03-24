Private libraries using the same library name, in the same context, defined in
the same folder. One of them is unavailable because it's `(optional)` and a
dependency is missing.

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (libraries xxx)
  >  (optional))
  > (library
  >  (name foo))
  > EOF
  $ cat > foo.ml << EOF
  > let x = "hello"
  > EOF

Without any consumers of the libraries

  $ dune build

With some consumer of the library

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (modules foo)
  >  (libraries xxx)
  >  (optional))
  > (library
  >  (modules foo)
  >  (name foo))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline Foo.x
  > EOF

  $ dune build
