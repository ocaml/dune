the name field can be omitted for libraries when public_name is present
  $ dune build --root no-name-lib
  Entering directory 'no-name-lib'

this isn't possible for older syntax <= (1, 0)
  $ dune build --root no-name-lib-syntax-1-0
  File "dune", line 1, characters 22-25:
  Error: name field cannot be omitted before version 1.1 of the dune language
  [1]

executable(s) stanza works the same way

  $ dune build --root no-name-exes
  Entering directory 'no-name-exes'

  $ dune build --root no-name-exes-syntax-1-0
  File "dune", line 1, characters 0-36:
  Error: name field may not be omitted before dune version 1.1
  [1]

there's only a public name but it's invalid as a name

  $ dune build --root public-name-invalid-name
  Entering directory 'public-name-invalid-name'
      ocamlopt .c.find.objs/c.find.{cmx,o} (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.06.1/bin/ocamlopt.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -w -49 -g -I .c.find.objs -intf-suffix .ml-gen -no-alias-deps -o .c.find.objs/c.find.cmx -c -impl c.find.ml-gen)
  File "c.find.ml-gen", line 1:
  Error: Could not find the .cmi file for interface c.find.ml-gen.
  [1]
