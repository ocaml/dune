An emit stanza can use dependencies re-exported by its libraries, even when
implicit transitive dependencies are disabled. Re-exports from libraries and
melange.libraries compose transitively.

  $ make_melange_project 3.26 1.0
  $ cat >>dune-project <<EOF
  > (implicit_transitive_deps false)
  > EOF
  $ mkdir dep middle top app
  $ cat >dep/dune <<EOF
  > (library (name dep) (modes melange))
  > EOF
  $ cat >dep/dep.ml <<EOF
  > let value = 42
  > EOF
  $ cat >middle/dune <<EOF
  > (library (name middle) (modes melange)
  >  (libraries (re_export dep)))
  > EOF
  $ cat >middle/middle.ml <<EOF
  > let value = Dep.value + 1
  > EOF
  $ cat >top/dune <<EOF
  > (library (name top) (modes melange)
  >  (melange.libraries (re_export middle)))
  > EOF
  $ cat >top/top.ml <<EOF
  > let value = Middle.value
  > EOF
  $ cat >app/dune <<EOF
  > (melange.emit (target output) (libraries top))
  > EOF
  $ cat >app/main.ml <<EOF
  > let () =
  >   Js.log Dep.value;
  >   Js.log Middle.value;
  >   Js.log Top.value
  > EOF
  $ dune build @app/melange
  $ node _build/default/app/output/app/main.js
  42
  43
  43

The re_export form is rejected in an emit stanza itself.

  $ cat >app/dune <<EOF
  > (melange.emit (target output) (libraries (re_export top)))
  > EOF
  $ dune build @app/melange
  File "app/dune", line 1, characters 41-56:
  1 | (melange.emit (target output) (libraries (re_export top)))
                                               ^^^^^^^^^^^^^^^
  Error: re_export is not allowed here
  [1]
