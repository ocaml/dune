Compile a library with `(stdlib ..)` and wrapped settings

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF

  $ write_stdlib_mystdlib_sources
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
