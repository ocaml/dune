When (include_subdirs qualified) is enabled, we should forbid the same module
to be defined by both a normal compilation unit and a directory of modules

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name mylib))
  > EOF

  $ mkdir foo/
  $ touch foo.ml foo/bar.ml

  $ dune build @check --display short
      ocamldep .mylib.objs/mylib__Foo__Bar.impl.d
        ocamlc .mylib.objs/byte/mylib.{cmi,cmo,cmt}
        ocamlc .mylib.objs/byte/mylib__Foo.{cmi,cmo,cmt}
        ocamlc .mylib.objs/byte/mylib__Foo__Bar.{cmi,cmo,cmt}

  $ touch foo/foo.ml
  $ dune build @check --display short
      ocamldep .mylib.objs/mylib__Foo.impl.d
        ocamlc .mylib.objs/byte/mylib__Foo__.{cmi,cmo,cmt}
        ocamlc .mylib.objs/byte/mylib__Foo__Bar.{cmi,cmo,cmt}
        ocamlc .mylib.objs/byte/mylib__Foo.{cmi,cmo,cmt}
