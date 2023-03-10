Test that inter-module dependencies are tracked by melange.emit

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist))
  > EOF

  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > bar.ml <<EOF
  > let x = Foo.x
  > EOF

  $ dune build dist/bar.js --display short
      ocamldep .dist.mobjs/melange__Bar.impl.d
      ocamldep .dist.mobjs/melange__Foo.impl.d
          melc .dist.mobjs/melange/melange.{cmi,cmj,cmt}
          melc .dist.mobjs/melange/melange__Foo.{cmi,cmj,cmt}
          melc .dist.mobjs/melange/melange__Bar.{cmi,cmj,cmt}
          melc dist/bar.js

  $ node _build/default/dist/bar.js 2>&1 | grep 'Cannot find'
  Error: Cannot find module './foo.js'
