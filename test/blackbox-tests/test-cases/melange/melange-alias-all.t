Melange compilation is added to `@all`

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using melange 1.0)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mlib)
  >  (modes melange))
  > EOF
  $ cat > lib/hello.ml <<EOF
  > let x = "hello"
  > EOF

  $ dune build @all
  $ find _build/default | grep '\.cm' | sort
  _build/default/lib/.mlib.objs/melange/mlib.cmi
  _build/default/lib/.mlib.objs/melange/mlib.cmj
  _build/default/lib/.mlib.objs/melange/mlib.cmt
  _build/default/lib/.mlib.objs/melange/mlib__Hello.cmi
  _build/default/lib/.mlib.objs/melange/mlib__Hello.cmj
  _build/default/lib/.mlib.objs/melange/mlib__Hello.cmt
