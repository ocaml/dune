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

Build again. Even though `foo.mli` did not change, `x.ml` is recompiled
because Melange compilation depends on the dependency library's `.cmj` files
for implementation information such as cross-module optimization.

  $ dune build
  $ dune trace cat | jq_dune -s '[.[] | targetsMatchingFilter(test("melange__X"))] | length'
  1
  $ node ./_build/default/out/x.js
  hi
