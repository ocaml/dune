public libraries may not have private dependencies

  $ $JBUILDER build -j1 --display short --root private-dep 2>&1 | grep -v Entering
      ocamldep publiclib.ml.d
      ocamldep privatelib.ml.d
        ocamlc .privatelib.objs/privatelib.{cmi,cmo,cmt}
        ocamlc .publiclib.objs/publiclib.{cmi,cmo,cmt}
        ocamlc publiclib.cma
      ocamlopt .privatelib.objs/privatelib.{cmx,o}
      ocamlopt .publiclib.objs/publiclib.{cmx,o}
      ocamlopt publiclib.{a,cmxa}
      ocamlopt publiclib.cmxs

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
        ocamlc .private_ppx.objs/private_ppx.{cmi,cmo,cmt}
      ocamlopt .private_ppx.objs/private_ppx.{cmx,o}
      ocamlopt private_ppx.{a,cmxa}
      ocamlopt .ppx/private_ppx@mylib/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.pp.ml.d
      ocamldep private_runtime_dep.ml.d
        ocamlc .private_runtime_dep.objs/private_runtime_dep.{cmi,cmo,cmt}
        ocamlc .mylib.objs/mylib.{cmi,cmo,cmt}
        ocamlc mylib.cma
      ocamlopt .private_runtime_dep.objs/private_runtime_dep.{cmx,o}
      ocamlopt .mylib.objs/mylib.{cmx,o}
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib.cmxs

However, public binaries may accept private dependencies
  $ $JBUILDER build -j1 --display short --root exes 2>&1 | grep -v Entering
      ocamldep publicbin.ml.d
        ocamlc .publicbin.eobjs/publicbin.{cmi,cmo,cmt}
      ocamlopt .publicbin.eobjs/publicbin.{cmx,o}
      ocamlopt publicbin.exe
