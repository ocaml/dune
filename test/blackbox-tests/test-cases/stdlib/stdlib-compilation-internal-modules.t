Compile a library with `(stdlib ..)` and multiple globs for internal_modules

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF

  $ mkdir stdlib
  $ cat > stdlib/other.ml <<EOF
  > let other () = Mystdlib.defined_in_stdlib
  > EOF
  $ cat > stdlib/one_module.ml <<EOF
  > let foo = "foo"
  > EOF
  $ cat > stdlib/mystdlib.ml <<EOF
  > let defined_in_stdlib = "defined"
  > module One_module = One_module
  > module Other = Other
  > EOF

  $ cat >stdlib/dune <<EOF
  > (library
  >  (name mystdlib)
  >  (stdlib
  >   (internal_modules one* other)))
  > EOF

Works when setting the correct version

  $ dune build
  $ find _build/default/stdlib -iname '*.cmi' | sort;
  _build/default/stdlib/.mystdlib.objs/byte/mystdlib.cmi
  _build/default/stdlib/.mystdlib.objs/byte/mystdlib__One_module.cmi
  _build/default/stdlib/.mystdlib.objs/byte/mystdlib__Other.cmi

