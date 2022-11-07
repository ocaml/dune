A library can be shadowed by an internal module name:

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ mkdir lib1 lib2

  $ cat >lib1/dune <<EOF
  > (library
  >  (name lib1)
  >  (modes melange))
  > EOF
  $ cat >lib1/lib1.ml <<EOF
  > let greeting = "Hello World"
  > EOF

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries lib1)
  >  (name lib2)
  >  (modes melange))
  > EOF

Now we shadow lib1:
  $ cat >lib2/lib1.ml <<EOF
  > let greeting = ()
  > EOF
  $ cat >lib2/lib2.ml <<EOF
  > print_endline Lib1.greeting
  > EOF

  $ dune build lib2/.lib2.objs/melange/lib2.cmj
  File "lib2/lib2.ml", line 1, characters 14-27:
  1 | print_endline Lib1.greeting
                    ^^^^^^^^^^^^^
  Error: This expression has type unit but an expression was expected of type
           string
  [1]

We can use root_module to use lib1 with a different name:

  $ cat >lib2/dune <<EOF
  > (library
  >  (libraries lib1)
  >  (root_module root)
  >  (name lib2)
  >  (modes melange))
  > EOF
  $ cat >lib2/lib2.ml <<EOF
  > let () = print_endline Root.Lib1.greeting
  > EOF
  $ dune build lib2/.lib2.objs/melange/lib2.cmj

The same for melange.emit:

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (libraries lib1)
  >  (root_module root)
  >  (module_system commonjs))
  > EOF
  $ cat > lib1.ml <<EOF
  > let greeting = ()
  > EOF
  $ cat >foo.ml <<EOF
  > print_endline Lib1.greeting
  > EOF
  $ dune build output/melange__Foo.js
  File "foo.ml", line 1, characters 14-27:
  1 | print_endline Lib1.greeting
                    ^^^^^^^^^^^^^
  Error: This expression has type unit but an expression was expected of type
           string
  [1]

Use root_module to fix:

  $ cat >foo.ml <<EOF
  > print_endline Root.Lib1.greeting
  > EOF
  $ dune build output/melange__Foo.js
  $ node _build/default/output/melange__Foo.js
  Hello World
