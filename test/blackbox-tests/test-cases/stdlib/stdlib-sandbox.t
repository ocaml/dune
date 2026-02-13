Show interaction between `(library (stdlib ..))` and sandboxed builds

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > (package (name mystdlib))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (public_name mystdlib)
  >  (name mystdlib)
  >  (modules_without_implementation printf buffer)
  >  (stdlib
  >   (modules_before_stdlib CamlinternalFormatBasics)
  >   (internal_modules Camlinternal*)))
  > EOF

  $ cat > mystdlib.mli <<EOF
  > type out_channel
  > type ('a, 'b, 'c) format =
  >   ('a, 'b, 'c, 'c, 'c, 'c) CamlinternalFormatBasics.format6
  > type ('a, 'b, 'c, 'd) format4 =
  >   ('a, 'b, 'c, 'd, 'd, 'd) CamlinternalFormatBasics.format6
  > module Buffer = Mystdlib__Buffer
  > module Printf = Mystdlib__Printf
  > EOF
  $ cat > mystdlib.ml <<EOF
  > type out_channel
  > type ('a, 'b, 'c) format =
  >   ('a, 'b, 'c, 'c, 'c, 'c) CamlinternalFormatBasics.format6
  > type ('a, 'b, 'c, 'd) format4 =
  >   ('a, 'b, 'c, 'd, 'd, 'd) CamlinternalFormatBasics.format6
  > module Buffer = Mystdlib__Buffer
  > module Printf = Mystdlib__Printf
  > EOF

  $ cat > buffer.mli <<EOF
  > type t
  > EOF
  $ cat > consumer.ml <<EOF
  > let _ = Printf.sprintf "%d" 1
  > EOF

  $ where="$(ocamlc -where)"
  $ cp "$where/camlinternalFormatBasics.mli" .
  $ cp "$where/camlinternalFormatBasics.ml" .
  $ cp "$where/printf.mli" .

  $ DUNE_SANDBOX=none dune build

Fails to pick up the hidden stdlib deps in the sandbox

  $ DUNE_SANDBOX=symlink dune build
