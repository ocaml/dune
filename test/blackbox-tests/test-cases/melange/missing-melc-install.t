Test multi-mode single context installed packages when melc is not available

Set up an environment that deliberately hides melc.

  $ mkdir _path
  $ for bin in dune ocamlc ocamldep ocamlopt ocamlobjinfo git gcc ar as ranlib \
  >   clang cc ld; do
  >   if command -v "$bin" > /dev/null; then
  >     ln -s "$(command -v "$bin")" _path/
  >   fi
  > done

Installed mixed-mode libraries are downgraded to their OCaml modes and remain
usable by installed consumers.

A bytecode and native library with Melange mode keeps both OCaml modes.

  $ mkdir package-byte-native
  $ cat > package-byte-native/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name pkg-byte-native))
  > (using melange 1.0)
  > EOF
  $ cat > package-byte-native/dune <<EOF
  > (library
  >  (public_name pkg-byte-native)
  >  (name mylib)
  >  (modules mylib)
  >  (modes :standard melange))
  > EOF
  $ cat > package-byte-native/mylib.ml <<EOF
  > let t = "hello from pkg-byte-native"
  > EOF

  $ mkdir consumer-byte-native
  $ cat > consumer-byte-native/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat > consumer-byte-native/dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries pkg-byte-native))
  > EOF
  $ cat > consumer-byte-native/main.ml <<EOF
  > let () = print_endline Mylib.t
  > EOF

  $ (PATH=$PWD/_path dune build --root package-byte-native @install) && \
  > (PATH=$PWD/_path dune install --root package-byte-native --prefix "$PWD/prefix") && \
  > (PATH=$PWD/_path OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build \
  >    --root consumer-byte-native main.exe)
  Entering directory 'package-byte-native'
  File ".mylib.objs/melange/_unknown_", line 1, characters 0-0:
  Error: Program melc not found in the tree or in PATH
   (context: default)
  Hint: opam install melange
  Leaving directory 'package-byte-native'
  [1]

Melange-only libraries still require melc at the package boundary.

  $ mkdir package-melange-only
  $ cat > package-melange-only/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name pkg-melange-only))
  > (using melange 1.0)
  > EOF
  $ cat > package-melange-only/dune <<EOF
  > (library
  >  (public_name pkg-melange-only)
  >  (name mylib)
  >  (modules mylib)
  >  (modes melange))
  > EOF
  $ cat > package-melange-only/mylib.ml <<EOF
  > let t = "hello from pkg-melange-only"
  > EOF

  $ (PATH=$PWD/_path dune build --root package-melange-only @install)
  Entering directory 'package-melange-only'
  File ".mylib.objs/melange/_unknown_", line 1, characters 0-0:
  Error: Program melc not found in the tree or in PATH
   (context: default)
  Hint: opam install melange
  Leaving directory 'package-melange-only'
  [1]
