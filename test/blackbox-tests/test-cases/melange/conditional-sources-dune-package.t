Test that paths in `node_modules` are correct for sub-libraries of the
form `foo.bar.baz`

  $ mkdir a app
  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name a)
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/melange_only.melange.ml <<EOF
  > let x = "melange"
  > EOF


  $ dune build --root a

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.a
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cma
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a__Foo.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Melange_only.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Melange_only.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Melange_only.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/melange_only.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmxs

  $ cat prefix/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name a)
  (sections
   (lib
    $TESTCASE_ROOT/prefix/lib/a)
   (libexec
    $TESTCASE_ROOT/prefix/lib/a))
  (files
   (lib
    (META
     dune-package
     sub/a.a
     sub/a.cma
     sub/a.cmi
     sub/a.cmt
     sub/a.cmx
     sub/a.cmxa
     sub/a.ml
     sub/a__Foo.cmi
     sub/a__Foo.cmt
     sub/a__Foo.cmx
     sub/foo.ml
     sub/melange/a.cmi
     sub/melange/a.cmj
     sub/melange/a.cmt
     sub/melange/a.ml
     sub/melange/a__Foo.cmi
     sub/melange/a__Foo.cmj
     sub/melange/a__Foo.cmt
     sub/melange/a__Melange_only.cmi
     sub/melange/a__Melange_only.cmj
     sub/melange/a__Melange_only.cmt
     sub/melange/foo.ml
     sub/melange/melange_only.ml))
   (libexec (sub/a.cmxs)))
  (library
   (name a.sub)
   (kind normal)
   (archives (byte sub/a.cma) (native sub/a.cmxa))
   (plugins (byte sub/a.cma) (native sub/a.cmxs))
   (native_archives sub/a.a)
   (main_module_name A)
   (modes melange byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name a)
       (visibility public)
       (kind alias)
       (source (path A) (impl (path sub/a.ml-gen))))
      (name A)
      (modules
       (module
        (obj_name a__Foo)
        (visibility public)
        (source (path Foo) (impl (path sub/foo.ml))))))
     (wrapped true)))
   (melange_modules
    (wrapped
     (group
      (alias
       (obj_name a)
       (visibility public)
       (kind alias)
       (source (path A) (impl (path sub/a.ml-gen))))
      (name A)
      (modules
       (module
        (obj_name a__Foo)
        (visibility public)
        (source (path Foo) (impl (path sub/foo.ml))))
       (module
        (obj_name a__Melange_only)
        (visibility public)
        (source (path Melange_only) (impl (path sub/melange_only.ml))))))
     (wrapped true))))

  $ cat >app/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries a.sub))
  > EOF

  $ cat > app/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @dist --display short 2>&1
  Entering directory 'app'
          melc dist/node_modules/a.sub/a.js
          melc dist/node_modules/a.sub/foo.js
          melc dist/node_modules/a.sub/melange_only.js
          melc .dist.mobjs/melange/melange__Bar.{cmi,cmj,cmt}
          melc dist/bar.js
  Leaving directory 'app'


  $ find app/_build/default/dist/node_modules/a.sub -type f | sort
  app/_build/default/dist/node_modules/a.sub/a.js
  app/_build/default/dist/node_modules/a.sub/foo.js
  app/_build/default/dist/node_modules/a.sub/melange_only.js
