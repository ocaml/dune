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

First we test wrapped:

  $ cat >stdlib/dune <<EOF
  > (library
  >  (name mystdlib)
  >  (stdlib
  >   (internal_modules one* other)))
  > EOF
  $ dune build
  File "stdlib/dune", line 4, characters 25-30:
  4 |   (internal_modules one* other)))
                               ^^^^^
  Error: Too many arguments for internal_modules
  [1]
  $ find _build/default/stdlib -iname '*.cmi' | sort;
  find: '_build/default/stdlib': No such file or directory

