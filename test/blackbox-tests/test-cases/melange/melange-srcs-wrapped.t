Inspect build dir path of generated melange sources under
`(include_subdirs unqualified)` and `(wrapped true)`

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package  (name pkg))
  > (using melange 1.0)
  > EOF

  $ mkdir a
  $ cat > a/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name a)
  >  (package pkg)
  >  (modes melange byte))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/bar.ml <<EOF
  > let x = "bar"
  > EOF

  $ dune build a/.melange_src/foo.ml
  $ dune build _build/default/a/.a.objs/melange/a.cmi
  $ find a _build/default/a -type f | sort
  _build/default/a/.a.objs/melange/a.cmi
  _build/default/a/.a.objs/melange/a.cmj
  _build/default/a/.a.objs/melange/a.cmt
  _build/default/a/.melange_src/a.ml-gen
  _build/default/a/.melange_src/foo.ml
  _build/default/a/.merlin-conf/lib-a
  _build/default/a/foo.ml
  a/bar.ml
  a/dune
  a/foo.ml

  $ dune build
  $ find a _build/default/a -type f | sort
  _build/default/a/.a.objs/a__Bar.impl.all-deps
  _build/default/a/.a.objs/a__Foo.impl.all-deps
  _build/default/a/.a.objs/byte/a.cmi
  _build/default/a/.a.objs/byte/a.cmo
  _build/default/a/.a.objs/byte/a.cmt
  _build/default/a/.a.objs/byte/a__Bar.cmi
  _build/default/a/.a.objs/byte/a__Bar.cmo
  _build/default/a/.a.objs/byte/a__Bar.cmt
  _build/default/a/.a.objs/byte/a__Foo.cmi
  _build/default/a/.a.objs/byte/a__Foo.cmo
  _build/default/a/.a.objs/byte/a__Foo.cmt
  _build/default/a/.a.objs/melange/a.cmi
  _build/default/a/.a.objs/melange/a.cmj
  _build/default/a/.a.objs/melange/a.cmt
  _build/default/a/.a.objs/melange/a__Bar.cmi
  _build/default/a/.a.objs/melange/a__Bar.cmj
  _build/default/a/.a.objs/melange/a__Bar.cmt
  _build/default/a/.a.objs/melange/a__Bar.impl.all-deps
  _build/default/a/.a.objs/melange/a__Foo.cmi
  _build/default/a/.a.objs/melange/a__Foo.cmj
  _build/default/a/.a.objs/melange/a__Foo.cmt
  _build/default/a/.a.objs/melange/a__Foo.impl.all-deps
  _build/default/a/.melange_src/a.ml-gen
  _build/default/a/.melange_src/bar.ml
  _build/default/a/.melange_src/foo.ml
  _build/default/a/.merlin-conf/lib-a
  _build/default/a/a.cma
  _build/default/a/a.ml-gen
  _build/default/a/bar.ml
  _build/default/a/foo.ml
  a/bar.ml
  a/dune
  a/foo.ml

