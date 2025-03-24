Test what happens when melange.emit stanza depends on non-Melange libraries

  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main_melange)
  >  (libraries foo))
  > (executable
  >  (name main_native)
  >  (modules main_native)
  >  (libraries foo))
  > EOF

  $ cat > main_melange.ml <<EOF
  > let _ = Foo.t
  > EOF

  $ cat > main_native.ml <<EOF
  > let _ = Foo.t
  > EOF

  $ mkdir lib

  $ cat > lib/dune <<EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > lib/Foo.ml <<EOF
  > let t = "Hello World"
  > EOF

Building the native executable does not fail

  $ dune build ./main_native.exe

But building melange does

  $ dune build @melange
  File "lib/dune", lines 1-2, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: The library `foo` was added as a dependency of a `melange.emit`
  stanza, but this library is not compatible with Melange. To fix this, add
  `melange` to the `modes` field of the library `foo`.
  [1]

Check the transitive case

  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (modes :standard melange)
  >  (libraries bar))
  > EOF

  $ cat > lib/Foo.ml <<EOF
  > let t = Bar.t
  > EOF

  $ mkdir lib2

  $ cat > lib2/dune <<EOF
  > (library
  >  (name bar))
  > EOF

  $ cat > lib/Bar.ml <<EOF
  > let t = "Hello World"
  > EOF

  $ dune build @melange
  File "lib2/dune", lines 1-2, characters 0-21:
  1 | (library
  2 |  (name bar))
  Error: The library `bar` was added as a dependency of a `melange.emit`
  stanza, but this library is not compatible with Melange. To fix this, add
  `melange` to the `modes` field of the library `bar`.
  [1]
