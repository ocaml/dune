Compile a library with `(stdlib ..)` and wrapped settings

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF

  $ mkdir stdlib
  $ runtest() {
  > cat >stdlib/dune <<EOF
  > (library
  >  (name mystdlib)
  >  (wrapped $1)
  >  (stdlib
  >   (internal_modules Camlinternal*)))
  > EOF
  > dune build
  > find _build/default/stdlib -iname '*.cmi' | sort;
  > }

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

First we test wrapped:

  $ runtest "true"
  _build/default/stdlib/.mystdlib.objs/byte/mystdlib.cmi
  _build/default/stdlib/.mystdlib.objs/byte/mystdlib__One_module.cmi
  _build/default/stdlib/.mystdlib.objs/byte/mystdlib__Other.cmi

And now unwrapped:

  $ runtest "false"
  _build/default/stdlib/.mystdlib.objs/byte/mystdlib.cmi
  _build/default/stdlib/.mystdlib.objs/byte/one_module.cmi
  _build/default/stdlib/.mystdlib.objs/byte/other.cmi
