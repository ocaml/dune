Test to showcase "inconsistent assumption" issues when using melange ppx

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (libraries my_lib))
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name my_lib)
  >  (modes melange))
  > EOF

  $ cat > main.ml <<EOF
  > let t = My_lib.A.t
  > let u = My_lib.B.t
  > EOF

  $ cat > lib/a.ml <<EOF
  > let t = D.t
  > let u = C.t
  > EOF

  $ cat > lib/b.ml <<EOF
  > let t = [%bs.obj { a = C.t }]
  > EOF

  $ cat > lib/c.ml <<EOF
  > type t = < a : D.t >
  > let t: < a : D.t > = [%bs.obj { a = D.t }]
  > EOF

  $ cat > lib/d.ml <<EOF
  > type t = string
  > let t = "bar"
  > EOF

  $ dune build @mel
  File "lib/b.ml", line 1, characters 23-26:
  1 | let t = [%bs.obj { a = C.t }]
                             ^^^
  Error: The module C is an alias for module My_lib__C, which is missing
  [1]

Now change D which is a leaf in the dep tree, notice error on revdeps A (direct)
and B (transitive)

  $ cat > lib/d.ml <<EOF
  > type t = int
  > let t = 2
  > EOF

  $ dune build @mel
  File "_none_", line 1:
  Error: My_lib__C not found, it means either the module does not exist or it is a namespace
  File "main.ml", line 1:
  Error: The files lib/.my_lib.objs/melange/my_lib__A.cmi
         and lib/.my_lib.objs/melange/my_lib__B.cmi
         make inconsistent assumptions over interface My_lib__D
  [1]

Now replace modules with melange-ppx-less code

  $ cat > lib/b.ml <<EOF
  > let t = C.t
  > EOF

  $ cat > lib/c.ml <<EOF
  > type t = D.t
  > let t: D.t = D.t
  > EOF

  $ dune build @mel
