  $ dune build --display short @install --debug-dep
  File "_build/default/.ppx/a.kernel/ppx.exe", line 1, characters 0-0:
  Error: Failed to create on-demand ppx rewriter for a.kernel; no ppx driver
  were found.
  Hint: Try upgrading or reinstalling ocaml-migrate-parsetree and ppxlib.
  -> required by (pps (a.kernel))
  -> required by .ppx/a.kernel/_ppx.ml
  -> required by .ppx/a.kernel/ppx.exe
  -> required by install lib/a/kernel/ppx.exe
  -> required by a/a.install
  -> required by alias a/install
  File "_build/default/.ppx/a/ppx.exe", line 1, characters 0-0:
  Error: Failed to create on-demand ppx rewriter for a; no ppx driver were
  found.
  Hint: Try upgrading or reinstalling ocaml-migrate-parsetree and ppxlib.
  -> required by (pps (a))
  -> required by .ppx/a/_ppx.ml
  -> required by .ppx/a/ppx.exe
  -> required by install lib/a/ppx.exe
  -> required by a/a.install
  -> required by alias a/install
  File "b/dune", line 5, characters 2-9:
  5 |   (pps a)))
        ^^^^^^^
  Error: No ppx driver were found.
  Hint: Try upgrading or reinstalling ocaml-migrate-parsetree and ppxlib.
  -> required by b/b.pp.ml
  -> required by b/.b.objs/byte/b.cmi
  -> required by b/.b.objs/native/b.cmx
  -> required by b/b$ext_lib
  -> required by install lib/b/b$ext_lib
  -> required by b/b.install
  -> required by alias b/install
        ocamlc a/ppx/.a.objs/byte/a.{cmi,cmo,cmt}
        ocamlc a/ppx/a.cma
        ocamlc a/kernel/.a_kernel.objs/byte/a_kernel.{cmi,cmo,cmt}
        ocamlc a/kernel/a_kernel.cma
      ocamlopt a/ppx/.a.objs/native/a.{cmx,o}
      ocamlopt a/ppx/a.{a,cmxa}
      ocamlopt a/ppx/a.cmxs
      ocamlopt a/kernel/.a_kernel.objs/native/a_kernel.{cmx,o}
      ocamlopt a/kernel/a_kernel.{a,cmxa}
      ocamlopt a/kernel/a_kernel.cmxs
  [1]
