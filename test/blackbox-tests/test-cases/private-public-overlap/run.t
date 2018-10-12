public libraries may not have private dependencies

  $ dune build --display short --root private-dep
  Entering directory 'private-dep'
  File "dune", line 8, characters 12-22:
  8 |  (libraries privatelib)
                  ^^^^^^^^^^
  Error: Library "privatelib" is private, it cannot be a dependency of a public library.
  You need to give "privatelib" a public name.
      ocamldep .publiclib.objs/publiclib.ml.d
  [1]

On the other hand, public libraries may have private preprocessors
  $ dune build --display short --root private-rewriter
  Entering directory 'private-rewriter'
        ocamlc .ppx_internal.objs/ppx_internal.{cmi,cmo,cmt}
      ocamlopt .ppx_internal.objs/ppx_internal.{cmx,o}
      ocamlopt ppx_internal.{a,cmxa}
      ocamlopt .ppx/jbuild/a55edf08f347158c59e28648f66f5be3/ppx.exe
           ppx mylib.pp.ml
      ocamldep .mylib.objs/mylib.pp.ml.d
        ocamlc .mylib.objs/mylib.{cmi,cmo,cmt}
        ocamlc mylib.cma
      ocamlopt .mylib.objs/mylib.{cmx,o}
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib.cmxs

Unless they introduce private runtime dependencies:
  $ dune build --display short --root private-runtime-deps
  Entering directory 'private-runtime-deps'
  File "jbuild", line 16, characters 20-31:
  16 |   (preprocess (pps (private_ppx)))
                           ^^^^^^^^^^^
  Error: Library "private_runtime_dep" is private, it cannot be a dependency of a public library.
  You need to give "private_runtime_dep" a public name.
        ocamlc .private_ppx.objs/private_ppx.{cmi,cmo,cmt}
      ocamlopt .private_ppx.objs/private_ppx.{cmx,o}
      ocamlopt private_ppx.{a,cmxa}
      ocamlopt .ppx/jbuild/3e9c40969655ac7a27e8982841adf043/ppx.exe
           ppx mylib.pp.ml
      ocamldep .mylib.objs/mylib.pp.ml.d
  [1]

However, public binaries may accept private dependencies
  $ dune build --display short --root exes
  Entering directory 'exes'
      ocamldep .publicbin.eobjs/publicbin.ml.d
        ocamlc .publicbin.eobjs/publicbin.{cmi,cmo,cmt}
      ocamlopt .publicbin.eobjs/publicbin.{cmx,o}
      ocamlopt publicbin.exe

Private dependencies shouldn't make the library optional
  $ dune build --display short --root optional
  Entering directory 'optional'
