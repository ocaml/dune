https://github.com/ocaml/dune/issues/13299

Select with unavailable library should fallback, but optional vlib
implementation is incorrectly marked as having unavailable dependencies.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p mylib_opt
  $ cat > mylib_opt/dune << EOF
  > (library
  >  (name mylib_opt)
  >  (libraries external_lib_not_found))
  > EOF

  $ mkdir -p mylib_base
  $ cat > mylib_base/dune << EOF
  > (library
  >  (name mylib_base)
  >  (libraries
  >   (select config.ml from
  >    (mylib_opt -> config.with.ml)
  >    (-> config.without.ml))))
  > EOF
  $ touch mylib_base/config.with.ml
  $ touch mylib_base/config.without.ml

  $ mkdir -p mylib_virt
  $ cat > mylib_virt/dune << EOF
  > (library
  >  (name mylib_virt)
  >  (virtual_modules mylib_virt))
  > EOF
  $ touch mylib_virt/mylib_virt.mli

  $ mkdir -p mylib_impl
  $ cat > mylib_impl/dune << EOF
  > (library
  >  (name mylib_impl)
  >  (implements mylib_virt)
  >  (libraries mylib_base)
  >  (optional))
  > EOF
  $ touch mylib_impl/mylib_virt.ml

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries mylib_impl))
  > EOF
  $ touch main.ml

  $ dune build main.exe 2>&1

Forcing the fallback succeeds:

  $ cat > mylib_base/dune << EOF
  > (library
  >  (name mylib_base)
  >  (libraries
  >   (select config.ml from
  >    (-> config.without.ml))))
  > EOF
  $ dune build main.exe

Should still build the fallback in the negative case:

  $ cat > mylib_base/dune << EOF
  > (library
  >  (name mylib_base)
  >  (libraries
  >   (select config.ml from
  >    (mylib_opt -> config.with.ml)
  >    (!mylib_opt -> config.without.ml))))
  > EOF
  $ dune build main.exe
