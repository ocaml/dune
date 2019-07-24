  $ dune build --display short @install --debug-dep
  Error: invalid ppx key for default/.ppx/a.kernel/ppx.exe
  -> required by .ppx/a.kernel/ppx.exe
  -> required by install lib/a/kernel/ppx.exe
  -> required by a/a.install
  -> required by alias a/install
  Error: invalid ppx key for default/.ppx/a/ppx.exe
  -> required by .ppx/a/ppx.exe
  -> required by install lib/a/ppx.exe
  -> required by a/a.install
  -> required by alias a/install
        ocamlc a/ppx/.a.objs/byte/a.{cmi,cmo,cmt}
        ocamlc a/ppx/a.cma
        ocamlc a/kernel/.a_kernel.objs/byte/a_kernel.{cmi,cmo,cmt}
        ocamlc a/kernel/a_kernel.cma
      ocamlopt a/ppx/.a.objs/native/a.{cmx,o}
      ocamlopt a/ppx/a.{a,cmxa}
      ocamlopt a/ppx/a.cmxs
      ocamlopt a/kernel/.a_kernel.objs/native/a_kernel.{cmx,o}
      ocamlopt a/kernel/a_kernel.{a,cmxa}
      ocamlopt .ppx/631b31a68eb10e1850cf7721d41e5b84/ppx.exe
           ppx b/b.pp.ml
      ocamldep b/.b.objs/b.pp.ml.d
        ocamlc b/.b.objs/byte/b.{cmi,cmo,cmt}
        ocamlc b/b.cma
      ocamlopt a/kernel/a_kernel.cmxs
      ocamlopt b/.b.objs/native/b.{cmx,o}
      ocamlopt b/b.{a,cmxa}
      ocamlopt b/b.cmxs
  [1]
