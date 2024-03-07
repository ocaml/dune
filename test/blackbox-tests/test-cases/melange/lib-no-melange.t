Test what happens when melange.emit stanza depends on non-Melange libraries

  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
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
  File "dune", line 4, characters 1-16:
  4 |  (libraries foo))
       ^^^^^^^^^^^^^^^
  Error: The library "foo" was added as a dependency of a melange.emit stanza,
  but this library is not compatible with melange. To fix this, add (modes
  melange) to the library stanza.
  [1]

