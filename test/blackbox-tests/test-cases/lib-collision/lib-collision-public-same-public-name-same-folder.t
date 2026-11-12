Public libraries using the same library name, in the same context, defined in
the same folder.

  $ mkdir -p a b

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name bar) (allow_empty))
  > EOF

  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > (library
  >  (name bar)
  >  (public_name bar.foo))
  > EOF

Without any consumers of the libraries

  $ dune build
  Error: Multiple rules generated for
  _build/default/a/.merlin-conf/lib-bar.foo:
  - <internal location>
  - <internal location>
  -> required by _build/default/a/bar.a
  -> required by _build/install/default/lib/bar/foo/bar.a
  -> required by _build/default/bar.install
  -> required by alias all
  -> required by alias default
  File "a/dune", lines 1-3, characters 0-44:
  1 | (library
  2 |  (name foo)
  3 |  (public_name bar.foo))
  Error: Public library bar.foo is defined twice:
  - a/dune:4
  - a/dune:1
  [1]

With some consumer

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  Error: Multiple rules generated for
  _build/default/a/.merlin-conf/lib-bar.foo:
  - <internal location>
  - <internal location>
  -> required by _build/default/a/bar.a
  -> required by _build/install/default/lib/bar/foo/bar.a
  -> required by _build/default/bar.install
  -> required by alias all
  -> required by alias default
  File "a/dune", lines 1-3, characters 0-44:
  1 | (library
  2 |  (name foo)
  3 |  (public_name bar.foo))
  Error: Public library bar.foo is defined twice:
  - a/dune:4
  - a/dune:1
  File "a/dune", lines 4-6, characters 0-44:
  4 | (library
  5 |  (name bar)
  6 |  (public_name bar.foo))
  Error: Library with name "bar.foo" is already defined in a/dune:1. Either
  change one of the names, or enable them conditionally using the 'enabled_if'
  field.
  [1]

