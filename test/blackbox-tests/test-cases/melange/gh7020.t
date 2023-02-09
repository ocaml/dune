Reproduce github #7020

  $ dir=_to-install
  $ mkdir $dir
  $ cat >$dir/dune-project <<EOF
  > (lang dune 3.7)
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
  $ dune install --root . --prefix _install
  Installing _install/lib/dummyfoo/META
  Installing _install/lib/dummyfoo/dummyfoo.ml
  Installing _install/lib/dummyfoo/dune-package
  Installing _install/lib/dummyfoo/melange/dummyfoo.cmi
  Installing _install/lib/dummyfoo/melange/dummyfoo.cmj
  Installing _install/lib/dummyfoo/melange/dummyfoo.cmt
  $ cd ..

  $ export OCAMLPATH=$PWD/$dir/_install/lib

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat >dune <<EOF
  > (melange.emit
  >  (target es6)
  >  (alias melange)
  >  (libraries dummyfoo)
  >  (module_system es6))
  > EOF

  $ dune build @melange 2>&1 | awk '/Internal error/,/Raised/'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Option.value_exn", {})
  Raised at Stdune__Code_error.raise in file
