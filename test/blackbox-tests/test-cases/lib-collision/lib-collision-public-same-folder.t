Public libraries using the same library name, in the same context, defined in
the same folder.

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name bar) (allow_empty))
  > (package (name baz) (allow_empty))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > (library
  >  (name foo)
  >  (public_name baz.foo))
  > EOF

Without any consumers of the libraries

  $ dune build
  Error: Library foo is defined twice:
  - dune:6
  - dune:3
  [1]

With some consumer

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > (library
  >  (name foo)
  >  (public_name baz.foo))
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  Error: Library foo is defined twice:
  - dune:6
  - dune:3
  [1]
