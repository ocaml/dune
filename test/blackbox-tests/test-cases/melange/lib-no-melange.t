Test what happens when melange.emit stanza depends on non-Melange libraries

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main_melange)
  >  (libraries foo)
  >  (module_system commonjs))
  > (executable
  >  (name main_native)
  >  (modules main_native)
  >  (libraries foo)
  >  (modes native byte_complete))
  > EOF

  $ cat > main_melange.ml <<EOF
  > let t = Foo_Bar.t
  > EOF

  $ cat > main_native.ml <<EOF
  > let t = Foo_Bar.t
  > EOF

  $ mkdir lib

  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (wrapped false))
  > EOF

  $ cat > lib/Foo_Bar.ml <<EOF
  > let t = "Hello World"
  > EOF

  $ dune build output/melange__Main_melange.js
  File "dune", line 4, characters 12-15:
  4 |  (libraries foo)
                  ^^^
  Error: The library "foo" was added as a dependency of a melange.emit stanza,
  but this library is not compatible with melange. To fix this, add (modes
  melange) to the library stanza.
  [1]

But building the native executable does not fail

  $ dune build ./main_native.exe
  File "dune", line 4, characters 12-15:
  4 |  (libraries foo)
                  ^^^
  Error: The library "foo" was added as a dependency of a melange.emit stanza,
  but this library is not compatible with melange. To fix this, add (modes
  melange) to the library stanza.
  [1]
