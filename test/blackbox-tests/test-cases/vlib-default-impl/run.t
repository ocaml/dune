Test that trying to specify a default implementation for a non-virtual library results
in an appropriate error message.
  $ dune build --root default-impl-not-virtual-lib
  Entering directory 'default-impl-not-virtual-lib'
  File "dune", line 4, characters 25-33:
  4 |  (default_implementation lib.impl))
                               ^^^^^^^^
  Error: Only virtual libraries can specify a default implementation.
  [1]

Basic sample selecting implementation according to default library.
  $ dune build --root default-impl
  Entering directory 'default-impl'
  Multiple rules generated for _build/default/lib.default/.lib_default.objs/vlib.mli.all-deps:
  - src/virtual_rules.ml:42
  - <internal location>
  [1]

Check that ambiguity is handled correctly.
  $ dune build --root dependency-cycle
  Entering directory 'dependency-cycle'
  Error: Default implementation cycle detected between the following libraries:
     "clock"
  -> "clock_ocaml"
  -> "async_ocaml"
  -> "async"
  -> "async_c"
  -> "clock_c"
  -> "clock"
  -> "test_default"
  -> "test"
  -> required by executable bar in dune:2
  [1]

Check that default implementation data is installed in the dune package file.
  $ dune build --root dune-package
  Entering directory 'dune-package'
  $ cat dune-package/_build/install/default/lib/a/dune-package
  (lang dune 1.11)
  (name a)
  (library
   (name a)
   (kind normal)
   (virtual)
   (foreign_archives (native a$ext_lib))
   (default_implementation a-default)
   (main_module_name A)
   (modes byte native)
   (modules
    (alias_module (name A) (obj_name a) (visibility public) (kind alias) (impl))
    (main_module_name A)
    (modules
     ((name X) (obj_name a__X) (visibility public) (kind virtual) (intf)))
    (wrapped true)))

Test default implementation for an external library

First we create an external library and implementation
  $ dune build --root external/lib @install
  Entering directory 'external/lib'
  Multiple rules generated for _build/default/impl/.vlib_impl.objs/x.mli.all-deps:
  - src/virtual_rules.ml:42
  - <internal location>
  [1]

Then we make sure that it works fine.
  $ env OCAMLPATH=external/lib/_build/install/default/lib dune build --root external/exe --debug-dependency-path
  Entering directory 'external/exe'
  File "dune", line 2, characters 7-10:
  2 |  (name bar)
             ^^^
  Error: File unavailable: $TESTCASE_ROOT/external/lib/_build/install/default/lib/vlib/impl/vlib_impl$ext_lib
  -> required by
     $TESTCASE_ROOT/external/lib/_build/install/default/lib/vlib/impl/vlib_impl$ext_lib
  -> required by bar.exe
  -> required by alias default
  -> required by alias default
  File "dune", line 2, characters 7-10:
  2 |  (name bar)
             ^^^
  Error: File unavailable: $TESTCASE_ROOT/external/lib/_build/install/default/lib/vlib/impl/vlib_impl.cmxa
  -> required by
     $TESTCASE_ROOT/external/lib/_build/install/default/lib/vlib/impl/vlib_impl.cmxa
  -> required by bar.exe
  -> required by alias default
  -> required by alias default
  [1]
