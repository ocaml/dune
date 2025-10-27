Test that %{cmt:...} and %{cmti:...} variables work correctly.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

Create a library with both .ml and .mli files:

  $ cat > mylib.mli <<EOF
  > val hello : string
  > EOF

  $ cat > mylib.ml <<EOF
  > let hello = "world"
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib))
  > 
  > (rule
  >  (alias show-cmt)
  >  (deps %{cmt:mylib})
  >  (action
  >   (echo "cmt file: %{cmt:mylib}\n")))
  > 
  > (rule
  >  (alias show-cmti)
  >  (deps %{cmti:mylib})
  >  (action
  >   (echo "cmti file: %{cmti:mylib}\n")))
  > EOF

This feature is guarded behind dune lang 3.21:

  $ dune build @show-cmt @show-cmti
  File "dune", line 6, characters 7-19:
  6 |  (deps %{cmt:mylib})
             ^^^^^^^^^^^^
  Error: %{cmt:..} is only available since version 3.21 of the dune language.
  Please update your dune-project file to have (lang dune 3.21).
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Build and check that cmt and cmti files are found:

  $ dune build @show-cmt
  cmt file: .mylib.objs/byte/mylib.cmt

  $ dune build @show-cmti
  cmti file: .mylib.objs/byte/mylib.cmti

Test with a module that has only implementation (no interface):

  $ cat > only_impl.ml <<EOF
  > let x = 42
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias show-impl-only-cmt)
  >  (deps %{cmt:only_impl})
  >  (action
  >   (echo "impl-only cmt: %{cmt:only_impl}\n")))
  > 
  > (rule
  >  (alias show-impl-only-cmti)
  >  (deps %{cmti:only_impl})
  >  (action
  >   (echo "impl-only cmti: %{cmti:only_impl}\n")))
  > EOF

  $ dune build @show-impl-only-cmt
  impl-only cmt: .mylib.objs/byte/mylib__Only_impl.cmt

  $ dune build @show-impl-only-cmti
  Error: No rule found for .
  -> required by alias show-impl-only-cmti in dune:21
  [1]

Test with a module that has only interface (no implementation):

  $ cat > only_intf.mli <<EOF
  > val y : int
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (modules_without_implementation only_intf))
  > 
  > (rule
  >  (alias show-intf-only-cmt)
  >  (deps %{cmt:only_intf})
  >  (action
  >   (echo "intf-only cmt: %{cmt:only_intf}\n")))
  > 
  > (rule
  >  (alias show-intf-only-cmti)
  >  (deps %{cmti:only_intf})
  >  (action
  >   (echo "intf-only cmti: %{cmti:only_intf}\n")))
  > EOF

  $ dune build @show-intf-only-cmt
  Error: No rule found for .
  -> required by alias show-intf-only-cmt in dune:5
  [1]

  $ dune build @show-intf-only-cmti
  intf-only cmti: .mylib.objs/byte/mylib__Only_intf.cmti

Test error when module does not exist:

  $ cat >> dune <<EOF
  > (alias
  >  (name test-nonexistent)
  >  (deps %{cmt:nonexistent}))
  > EOF

  $ dune build @test-nonexistent
  File "dune", line 18, characters 7-25:
  18 |  (deps %{cmt:nonexistent}))
              ^^^^^^^^^^^^^^^^^^
  Error: Module Nonexistent does not exist.
  [1]

Test with native-only library (bytecode disabled):

  $ cat > native_lib.ml <<EOF
  > let z = 100
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name native_lib)
  >  (modules native_lib)
  >  (modes native))
  > 
  > (rule
  >  (alias show-native-cmt)
  >  (deps %{cmt:native_lib})
  >  (action
  >   (echo "native-lib cmt: %{cmt:native_lib}\n")))
  > EOF

  $ dune build @show-native-cmt --display short
        ocamlc .native_lib.objs/byte/native_lib.{cmi,cmo,cmt}
  native-lib cmt: .native_lib.objs/byte/native_lib.cmt

  $ cat > native_lib.mli <<EOF
  > val z : int
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias show-native-cmti)
  >  (deps %{cmti:native_lib})
  >  (action
  >   (echo "native-lib cmti: %{cmti:native_lib}\n")))
  > EOF

  $ dune build @show-native-cmti --display short
        ocamlc .native_lib.objs/byte/native_lib.{cmi,cmti}
  native-lib cmti: .native_lib.objs/byte/native_lib.cmti

