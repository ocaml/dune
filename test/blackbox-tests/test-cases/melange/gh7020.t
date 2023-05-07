Reproduce github #7020

  $ dir=_to-install
  $ mkdir $dir
  $ cat >$dir/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > (package
  >  (name dummyfoo))
  > EOF

  $ cat >$dir/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name dummyfoo))
  > EOF

  $ cat >$dir/META.dummyfoo.template <<EOF
  > # DUNE_GEN
  > EOF

  $ dune build @install --root $dir
  Entering directory '_to-install'
  Leaving directory '_to-install'
  $ cd $dir
  $ dune install --root . --prefix _install --display short
  Installing _install/lib/dummyfoo/META
  Installing _install/lib/dummyfoo/dummyfoo.ml
  Installing _install/lib/dummyfoo/dune-package
  Installing _install/lib/dummyfoo/melange/dummyfoo.cmi
  Installing _install/lib/dummyfoo/melange/dummyfoo.cmj
  Installing _install/lib/dummyfoo/melange/dummyfoo.cmt
  $ cd ..

  $ export OCAMLPATH=$PWD/$dir/_install/lib

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat >dune <<EOF
  > (melange.emit
  >  (target es6)
  >  (alias mel)
  >  (libraries dummyfoo)
  >  (emit_stdlib false)
  >  (module_systems es6))
  > EOF

  $ dune build @mel
  Error: The library dummyfoo was not compiled with Dune or it was compiled
  with Dune but published with a META template. Such libraries are not
  compatible with melange support
  -> required by alias mel
  [1]
