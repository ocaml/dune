Compile a library with `(stdlib ..)` and `(wrapped false)`

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF

  $ mkdir stdlib
  $ cat > stdlib/dune <<EOF
  > (library
  >  (name mystdlib)
  >  (wrapped false)
  >  (stdlib
  >   (internal_modules Camlinternal*)))
  > EOF
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

  $ dune build --display=short
      ocamldep stdlib/.mystdlib.objs/mystdlib.impl.d
      ocamldep stdlib/.mystdlib.objs/one_module.impl.d
      ocamldep stdlib/.mystdlib.objs/other.impl.d
        ocamlc stdlib/.mystdlib.objs/byte/mystdlib.{cmi,cmo,cmt}
      ocamlopt stdlib/.mystdlib.objs/native/mystdlib.{cmx,o}
        ocamlc stdlib/.mystdlib.objs/byte/one_module.{cmi,cmo,cmt}
        ocamlc stdlib/.mystdlib.objs/byte/other.{cmi,cmo,cmt}
      ocamlopt stdlib/.mystdlib.objs/native/one_module.{cmx,o}
      ocamlopt stdlib/.mystdlib.objs/native/other.{cmx,o}
        ocamlc stdlib/mystdlib.cma
      ocamlopt stdlib/mystdlib.{a,cmxa}
      ocamlopt stdlib/mystdlib.cmxs

modules are unwrapped

  $ ls _build/default/stdlib/.mystdlib.objs/native
  mystdlib.cmx
  mystdlib.o
  one_module.cmx
  one_module.o
  other.cmx
  other.o
