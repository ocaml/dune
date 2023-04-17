Test mdx stanza compatibility with Melange

  $ cat > dune << EOF
  > (mdx
  >  (files README.md)
  >  (libraries private_lib))
  > EOF

  $ DIR=lib
  $ mkdir $DIR

  $ cat >$DIR/dune << EOF
  > (library
  >  (name private_lib)
  >  (modules private_lib))
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (using mdx 0.3)
  > (using melange 0.1)
  > EOF

  $ cat >$DIR/private_lib.ml << EOF
  > let foo () = print_endline "foo"
  > EOF

  $ cat > README.md << 'EOF'
  > ```ocaml
  > # Private_lib.foo ();;
  > foo
  > - : unit = ()
  > ```
  > EOF

Using an OCaml lib works

  $ dune build @runtest

Let's change the library to have just melange mode

  $ cat >$DIR/dune << EOF
  > (library
  >  (name private_lib)
  >  (modules private_lib)
  >  (modes melange))
  > EOF

  $ cat >$DIR/private_lib.ml << EOF
  > let foo () = Js.log "foo"
  > EOF

  $ cat > README.md << 'EOF'
  > ```ocaml
  > # Private_lib.foo ();;
  > Line 1, characters 1-16:
  > Error: Unbound module Private_lib
  > ```
  > EOF

  $ dune build @runtest
