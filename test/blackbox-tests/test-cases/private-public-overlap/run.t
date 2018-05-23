public libraries may not have private dependencies

  $ dune build --display short --root private-dep 2>&1 | grep -v Entering
  File "dune", line 10, characters 14-24:
  Error: Library "privatelib" is private, it cannot be a dependency of a public library.
  You need to give "privatelib" a public name.
      ocamldep publiclib.ml.d

On the other hand, public libraries may have private preprocessors
  $ dune build --display short --root private-rewriter 2>&1 | grep -v Entering
        ocamlc .ppx_internal.objs/ppx_internal.{cmo,cmt,cmi}
      ocamlopt .ppx_internal.objs/ppx_internal.{cmx,o}
      ocamlopt ppx_internal.{cmxa,a}
      ocamlopt .ppx/ppx_internal@mylib/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.pp.ml.d
        ocamlc .mylib.objs/mylib.{cmt,cmi,cmo}
      ocamlopt .mylib.objs/mylib.{cmx,o}
      ocamlopt mylib.{cmxa,a}
      ocamlopt mylib.cmxs
        ocamlc mylib.cma

Unless they introduce private runtime dependencies:
  $ dune build --display short --root private-runtime-deps 2>&1 | grep -v Entering
  File "dune", line 16, characters 20-31:
  Error: Library "private_runtime_dep" is private, it cannot be a dependency of a public library.
  You need to give "private_runtime_dep" a public name.
        ocamlc .private_ppx.objs/private_ppx.{cmo,cmt,cmi}
      ocamlopt .private_ppx.objs/private_ppx.{cmx,o}
      ocamlopt private_ppx.{cmxa,a}
      ocamlopt .ppx/private_ppx@mylib/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.pp.ml.d

However, public binaries may accept private dependencies
  $ dune build --display short --root exes 2>&1 | grep -v Entering
      ocamldep publicbin.ml.d
        ocamlc .publicbin.eobjs/publicbin.{cmo,cmt,cmi}
      ocamlopt .publicbin.eobjs/publicbin.{cmx,o}
      ocamlopt publicbin.exe

Private dependencies shouldn't make the library optional
  $ dune build --display short --root optional 2>&1 | grep -v Entering
  [1]
