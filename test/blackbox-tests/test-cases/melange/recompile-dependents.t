Test Melange dependent recompilation

Set up and build a Melange library and `melange.emit`

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > EOF

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

  $ dune build --display short
          melc lib/.foo.objs/melange/foo.{cmj,cmt}
          melc .out.mobjs/melange/melange__X.{cmi,cmj,cmt}
          melc out/lib/foo.js
          melc out/x.js

  $ node ./_build/default/out/x.js
  hi
