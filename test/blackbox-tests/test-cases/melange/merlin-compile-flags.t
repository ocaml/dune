Show that the merlin config knows about melange.compile_flags

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (melange.compile_flags :standard -bs-D DEBUG=true )
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check

  $ dune ocaml merlin dump-config "$PWD" | grep -i "DEBUG"
  [1]

  $ cat >dune <<EOF
  > (melange.emit
  >  (target output)
  >  (compile_flags :standard -bs-D DEBUG=true ))
  > EOF

  $ dune build @check

  $ dune ocaml merlin dump-config "$PWD" | grep -i "DEBUG"
  [1]

