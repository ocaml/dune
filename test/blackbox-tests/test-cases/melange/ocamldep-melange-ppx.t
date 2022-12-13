Showcases issues with ocamldep when using melange ppx

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

We name the module Z so that its rules are processed after the ones from Main,
in order to trigger the error

  $ cat > z.ml <<EOF
  > let some_binding = Some "string"
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (target dist)
  >  (module_system commonjs))
  > EOF

  $ cat > main.ml <<EOF
  > let foo = [%bs.obj { foo = Z.some_binding }]
  > let () =
  >   Js.log foo
  > EOF

  $ dune build @dist
  File "main.ml", line 1, characters 27-41:
  1 | let foo = [%bs.obj { foo = Z.some_binding }]
                                 ^^^^^^^^^^^^^^
  Error: The module Z is an alias for module Melange__Z, which is missing
  [1]

The error above happens because ocamldep does not show dependencies on Main

  $ ocamldep -I . main.ml
  main.cmo :
  main.cmx :

A workaround is to alias the module so that ocamldep can find it,
so Z gets added to the dependencies of Main

  $ cat > main.ml <<EOF
  > module Z = Z
  > let foo = [%bs.obj { foo = Z.some_binding }]
  > let () =
  >   Js.log foo
  > EOF

  $ dune build @dist
  $ node _build/default/dist/main.js
  { foo: 'string' }

The above works as in that case, ocamldep picks up the dependency ok

  $ ocamldep -I . main.ml
  main.cmo : \
      z.cmo
  main.cmx : \
      z.cmx
