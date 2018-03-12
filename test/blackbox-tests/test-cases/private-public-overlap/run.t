public libraries may not have private dependencies

  $ $JBUILDER build -j1 --display short --root private-dep 2>&1 | grep -v Entering
  File "jbuild", line 10, characters 14-24:
  Error: Library "privatelib" is private, it cannot be a dependency of
  "publiclib" as the latter is a public library. You need to give "privatelib" a
  public name.

On the other hand, public libraries may have private preprocessors
  $ $JBUILDER build -j1 --display short --root private-rewriter 2>&1 | grep -v Entering
        ocamlc .ppx_internal.objs/ppx_internal.{cmi,cmo,cmt}
      ocamlopt .ppx_internal.objs/ppx_internal.{cmx,o}
      ocamlopt ppx_internal.{a,cmxa}
      ocamlopt .ppx/ppx_internal@mylib/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.pp.ml.d
        ocamlc .mylib.objs/mylib.{cmi,cmo,cmt}
      ocamlopt .mylib.objs/mylib.{cmx,o}
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib.cmxs
        ocamlc mylib.cma

Unless they introduce private runtime dependencies:
  $ $JBUILDER build -j1 --display short --root private-runtime-deps 2>&1 | grep -v Entering
  File "jbuild", line 14, characters 1-92:
  Error: Public libraries may not have private dependencies.
  Private dependency "private_runtime_dep" encountered in public library:

However, public binaries may accept private dependencies
  $ $JBUILDER build -j1 --display short --root exes 2>&1 | grep -v Entering
      ocamldep publicbin.ml.d
        ocamlc .publicbin.eobjs/publicbin.{cmi,cmo,cmt}
      ocamlopt .publicbin.eobjs/publicbin.{cmx,o}
      ocamlopt publicbin.exe
