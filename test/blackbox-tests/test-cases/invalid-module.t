Test `(include_subdirs qualified)` in the presence of invalid module names in
the source tree

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ cat > dune <<EOF
  > (library (name foo) (modules foo))
  > EOF
  $ touch foo.ml

Add `invalid-module.ml`, present in the source tree but not part of any
artifacts

  $ cat > invalid-module.ml <<EOF
  > print_endline "hello from invalid-module.ml"
  > EOF

  $ dune build

  $ cat > dune <<EOF
  > (library (name foo))
  > EOF
  $ dune build
  Error: foo__Invalid-module corresponds to an invalid module name
  -> required by _build/default/foo__.ml-gen
  -> required by alias all
  -> required by alias default
  [1]

Invalid module may be used in executables

  $ cat > dune <<EOF
  > (executable
  >  (name invalid-module)
  >  (modules invalid-module)
  >  (flags :standard -w -24))
  > EOF

  $ dune exec ./invalid-module.exe
  File "dune", line 2, characters 7-21:
  2 |  (name invalid-module)
             ^^^^^^^^^^^^^^
  Error: "invalid-module" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: invalid_module would be a correct module name
  [1]

Prior to Dune 3.0, executable names aren't validated as valid module names

  $ cat > dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ dune exec ./invalid-module.exe
  hello from invalid-module.ml
