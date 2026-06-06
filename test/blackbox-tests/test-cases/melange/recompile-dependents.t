Test Melange dependent recompilation

Set up and build a Melange library and `melange.emit`

  $ make_melange_project 3.13 0.1

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (modes melange))
  > EOF

  $ cat > lib/foo.ml <<EOF
  > let x () = "hello"
  > EOF
  $ cat > lib/foo.mli <<EOF
  > val x : unit -> string
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target out)
  >  (emit_stdlib false)
  >  (libraries foo))
  > EOF
  $ cat > x.ml <<EOF
  > let () = Js.log (Foo.x ())
  > EOF

  $ dune build
  $ node ./_build/default/out/x.js
  hello

Now change `foo.ml`, but keep `foo.mli` intact

  $ cat > lib/foo.ml <<EOF
  > let x () = "hi"
  > EOF

Build again, noting that x.ml could have been skipped?

  $ dune build
  $ node ./_build/default/out/x.js
  hi
