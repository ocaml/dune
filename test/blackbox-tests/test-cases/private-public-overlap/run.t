public libraries may not have private dependencies

  $ dune build --display short --root private-dep
  Entering directory 'private-dep'
  File "dune", line 8, characters 12-22:
  8 |  (libraries privatelib)
                  ^^^^^^^^^^
  Error: Library "privatelib" is private, it cannot be a dependency of a public
  library. You need to give "privatelib" a public name.
      ocamldep .publiclib.objs/publiclib.ml.d
  [1]

On the other hand, public libraries may have private preprocessors
  $ dune build --display short --root private-rewriter
  Entering directory 'private-rewriter'
        ocamlc .ppx_internal.objs/byte/ppx_internal.{cmi,cmo,cmt}
      ocamlopt .ppx_internal.objs/native/ppx_internal.{cmx,o}
      ocamlopt ppx_internal.{a,cmxa}
      ocamlopt .ppx/be26d3600214af2fa78c2c9ef25e9069/ppx.exe
           ppx mylib.pp.ml
      ocamldep .mylib.objs/mylib.pp.ml.d
        ocamlc .mylib.objs/byte/mylib.{cmi,cmo,cmt}
        ocamlc mylib.cma
      ocamlopt .mylib.objs/native/mylib.{cmx,o}
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib.cmxs

Unless they introduce private runtime dependencies:
  $ dune build --display short --root private-runtime-deps
  Entering directory 'private-runtime-deps'
  File "dune", line 16, characters 7-18:
  16 |   (pps private_ppx))
              ^^^^^^^^^^^
  Error: Library "private_runtime_dep" is private, it cannot be a dependency of
  a public library. You need to give "private_runtime_dep" a public name.
        ocamlc .private_ppx.objs/byte/private_ppx.{cmi,cmo,cmt}
      ocamlopt .private_ppx.objs/native/private_ppx.{cmx,o}
      ocamlopt private_ppx.{a,cmxa}
      ocamlopt .ppx/9ab0f533c010a2f808b2ba93118d36d0/ppx.exe
           ppx mylib.pp.ml
      ocamldep .mylib.objs/mylib.pp.ml.d
  [1]

However, public binaries may accept private dependencies
  $ dune build --display short --root exes
  Entering directory 'exes'
      ocamldep .publicbin.eobjs/publicbin.ml.d
        ocamlc .publicbin.eobjs/byte/publicbin.{cmi,cmo,cmt}
      ocamlopt .publicbin.eobjs/native/publicbin.{cmx,o}
      ocamlopt publicbin.exe

Private dependencies shouldn't make the library optional
  $ dune build @install --display short --root optional
  Entering directory 'optional'
