Show `(melange.modules ..)` subset is preferred when compiling in Melange mode

  $ mkdir -p a/lib
  $ cat >a/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name a))
  > (using melange 0.1)
  > EOF

  $ cat > a/lib/dune <<EOF
  > (library
  >  (public_name a)
  >  (melange.compile_flags -w +a-70)
  >  (modules foo helper)
  >  (melange.modules foo) ; helper isn't part of melange modules
  >  (modes melange byte))
  > EOF

  $ cat > a/lib/foo.ml <<EOF
  > let x = Helper.name
  > EOF
  $ cat > a/lib/helper.ml <<EOF
  > let name = "ocaml"
  > EOF
  $ cat > a/lib/foo.melange.ml <<EOF
  > let x = "melange"
  > EOF

  $ dune build --root a --display=short
  Entering directory 'a'
        ocamlc lib/.a.objs/byte/a.{cmi,cmo,cmt}
      ocamldep (internal)
      ocamldep (internal)
          melc lib/.a.objs/melange/a.{cmi,cmj,cmt}
      ocamldep (internal)
        ocamlc lib/.a.objs/byte/a__Helper.{cmi,cmo,cmt}
          melc lib/.a.objs/melange/a__Foo.{cmi,cmj,cmt}
        ocamlc lib/.a.objs/byte/a__Foo.{cmi,cmo,cmt}
        ocamlc lib/a.cma
  Leaving directory 'a'
  $ find a/_build/default/lib/.melange_src -type f | sort
  a/_build/default/lib/.melange_src/a.ml-gen
  a/_build/default/lib/.melange_src/foo.ml
  $ cat a/_build/default/lib/.melange_src/foo.ml
  # 1 "lib/foo.melange.ml"
  let x = "melange"

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cma
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Helper.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Helper.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/helper.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/foo.ml

  $ cat prefix/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name a)
  (sections
   (lib
    $TESTCASE_ROOT/prefix/lib/a))
  (files
   (lib
    (META
     a.cma
     a.cmi
     a.cmt
     a.ml
     a__Foo.cmi
     a__Foo.cmt
     a__Helper.cmi
     a__Helper.cmt
     dune-package
     foo.ml
     helper.ml
     melange/a.cmi
     melange/a.cmj
     melange/a.cmt
     melange/a.ml
     melange/a__Foo.cmi
     melange/a__Foo.cmj
     melange/a__Foo.cmt
     melange/foo.ml)))
  (library
   (name a)
   (kind normal)
   (archives (byte a.cma))
   (plugins (byte a.cma))
   (main_module_name A)
   (modes melange byte)
   (modules
    (wrapped
     (group
      (alias
       (obj_name a)
       (visibility public)
       (kind alias)
       (source (path A) (impl (path a.ml-gen))))
      (name A)
      (modules
       (module
        (obj_name a__Foo)
        (visibility public)
        (source (path Foo) (impl (path foo.ml))))
       (module
        (obj_name a__Helper)
        (visibility public)
        (source (path Helper) (impl (path helper.ml))))))
     (wrapped true)))
   (melange_modules
    (wrapped
     (group
      (alias
       (obj_name a)
       (visibility public)
       (kind alias)
       (source (path A) (impl (path a.ml-gen))))
      (name A)
      (modules
       (module
        (obj_name a__Foo)
        (visibility public)
        (source (path Foo) (impl (path foo.ml))))))
     (wrapped true))))
