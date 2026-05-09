Inspect build dir path of generated melange sources under
`(include_subdirs unqualified)` and `(wrapped false)`

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name pkg))
  > (using melange 1.0)
  > EOF

  $ mkdir a a/b a/b/c

  $ cat > a/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name a)
  >  (package pkg)
  >  (wrapped false)
  >  (modes melange byte))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/b/bar.ml <<EOF
  > let x = "bar"
  > EOF
  $ cat > a/b/c/qux.ml <<EOF
  > let x = "bar"
  > EOF

  $ dune build
  $ find _build/default/a -type f | sort
  _build/default/a/.a.objs/bar.impl.all-deps
  _build/default/a/.a.objs/byte/bar.cmi
  _build/default/a/.a.objs/byte/bar.cmo
  _build/default/a/.a.objs/byte/bar.cmt
  _build/default/a/.a.objs/byte/foo.cmi
  _build/default/a/.a.objs/byte/foo.cmo
  _build/default/a/.a.objs/byte/foo.cmt
  _build/default/a/.a.objs/byte/qux.cmi
  _build/default/a/.a.objs/byte/qux.cmo
  _build/default/a/.a.objs/byte/qux.cmt
  _build/default/a/.a.objs/foo.impl.all-deps
  _build/default/a/.a.objs/melange/bar.cmi
  _build/default/a/.a.objs/melange/bar.cmj
  _build/default/a/.a.objs/melange/bar.cmt
  _build/default/a/.a.objs/melange/bar.impl.all-deps
  _build/default/a/.a.objs/melange/foo.cmi
  _build/default/a/.a.objs/melange/foo.cmj
  _build/default/a/.a.objs/melange/foo.cmt
  _build/default/a/.a.objs/melange/foo.impl.all-deps
  _build/default/a/.a.objs/melange/qux.cmi
  _build/default/a/.a.objs/melange/qux.cmj
  _build/default/a/.a.objs/melange/qux.cmt
  _build/default/a/.a.objs/melange/qux.impl.all-deps
  _build/default/a/.a.objs/qux.impl.all-deps
  _build/default/a/.melange_src/b/bar.ml
  _build/default/a/.melange_src/b/c/qux.ml
  _build/default/a/.melange_src/foo.ml
  _build/default/a/.merlin-conf/lib-a
  _build/default/a/a.cma
  _build/default/a/b/bar.ml
  _build/default/a/b/c/qux.ml
  _build/default/a/foo.ml
