public libraries may not have private dependencies

  $ $JBUILDER build -j1 --display short --root private-dep 2>&1 | grep -v Entering
  File "jbuild", line 10, characters 14-24:
  Error: Library "privatelib" is private, it cannot be a dependency of
  "publiclib" as the latter is a public library. You need to give "privatelib" a
  public name.

On the other hand, public libraries may have private preprocessors
  $ $JBUILDER build -j1 --display short --root private-rewriter 2>&1 | grep -v Entering
  File "jbuild", line 1, characters 0-186:
  Error: Public libraries may not have private dependencies.
  Private dependency "ppx_internal" encountered in public library:
  -> required by library "mylib" in _build/default
        ocamlc .ppx_internal.objs/ppx_internal.{cmi,cmo,cmt}
      ocamlopt .ppx_internal.objs/ppx_internal.{cmx,o}
      ocamlopt ppx_internal.{a,cmxa}
      ocamlopt .ppx/ppx_internal@mylib/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.pp.ml.d

Unless they introduce private runtime dependencies:
  $ $JBUILDER build -j1 --display short --root private-runtime-deps 2>&1 | grep -v Entering
  File "jbuild", line 1, characters 0-327:
  Error: Public libraries may not have private dependencies.
  Private dependency "private_ppx" encountered in public library:
  -> required by library "mylib" in _build/default
        ocamlc .private_ppx.objs/private_ppx.{cmi,cmo,cmt}
      ocamlopt .private_ppx.objs/private_ppx.{cmx,o}
      ocamlopt private_ppx.{a,cmxa}
      ocamlopt .ppx/private_ppx@mylib/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.pp.ml.d

However, public binaries may accept private dependencies
  $ $JBUILDER build -j1 --display short --root exes 2>&1 | grep -v Entering
      ocamldep publicbin.ml.d
        ocamlc .publicbin.eobjs/publicbin.{cmi,cmo,cmt}
      ocamlopt .publicbin.eobjs/publicbin.{cmx,o}
      ocamlopt publicbin.exe
