Show that the merlin config knows about melange.compile_flags

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (melange.compile_flags :standard -w +42)
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check

  $ dune ocaml merlin dump-config "$PWD" | grep -i "+42"
     +42)))
     +42)))
     +42)))

  $ cat >dune <<EOF
  > (melange.emit
  >  (target output)
  >  (compile_flags :standard -w +42 ))
  > EOF

  $ dune build @check

  $ dune ocaml merlin dump-config "$PWD" | grep -i "+42"
     +42)))
     +42)))
     +42)))

