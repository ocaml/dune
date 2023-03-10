Test that inter-module dependencies are tracked by melange.emit

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name a)
  >  (modes melange))
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (entries)
  >  (libraries a))
  > EOF

  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > bar.ml <<EOF
  > let x = Foo.x
  > EOF

  $ dune build dist/bar.js --display short
      ocamldep .a.objs/a__Bar.impl.d
      ocamldep .a.objs/a__Foo.impl.d
          melc .a.objs/melange/a.{cmi,cmj,cmt}
          melc .a.objs/melange/a__Foo.{cmi,cmj,cmt}
          melc .a.objs/melange/a__Bar.{cmi,cmj,cmt}
          melc dist/bar.js

  $ node _build/default/dist/bar.js 2>&1 | grep 'Cannot find'
  Error: Cannot find module './foo.js'
