Test that trying to specify a variant for not an implementation results in an
appropriate error message.
  $ dune build --root variant-not-implementation
  Entering directory 'variant-not-implementation'
  File "dune", line 4, characters 10-13:
  4 |  (variant foo))
                ^^^
  Error: Only implementations can specify a variant.
  [1]

Having multiple implementations of the same library with respect to selected
variants results in an appropriate error message.
  $ dune build --root multiple-implementations-for-variants
  Entering directory 'multiple-implementations-for-variants'
  File "dune", line 4, characters 11-18:
  4 |  (variants default))
                 ^^^^^^^
  Error: Multiple solutions for the implementation
  of vlib  with variants [ "default" ]
  -> lib2_default ("default")
  -> lib_default ("default")
  -> required by executable bar in dune:2
  [1]

Basic sample using variants and a default library.
  $ dune build --root variants-base
  Entering directory 'variants-base'
           bar alias default
  hello from lib.test

Check that implementations are chosen according to manual specification, then
variants and finally default implementation.
  $ dune build --root resolution-priority
  Entering directory 'resolution-priority'
           bar alias default
  hi from direct.ocaml
  hi from variant.c
  hi from test.default

Check that variant data is installed in the dune package file.

  $ dune build --root dune-package
  Entering directory 'dune-package'
  $ cat  dune-package/_build/install/default/lib/a/dune-package
  (lang dune 1.11)
  (name a)
  (library
   (name a)
   (kind normal)
   (archives (byte a.cma) (native a.cmxa))
   (plugins (byte a.cma) (native a.cmxs))
   (foreign_archives (native a$ext_lib))
   (requires b)
   (implements b)
   (variant test)
   (main_module_name B)
   (modes byte native)
   (modules
    (alias_module (name B__a__) (obj_name b__a__) (visibility public) (impl))
    (main_module_name B)
    (modules ((name X) (obj_name b__X) (visibility public) (impl)))
    (wrapped true)))

Test variants for an external library

First we create an external library and implementation
  $ dune build --root external/lib @install
  Entering directory 'external/lib'

Then we make sure that it works fine.
  $ env OCAMLPATH=external/lib/_build/install/default/lib dune build --root external/exe --debug-dependency-path
  Entering directory 'external/exe'
           bar alias default
  hey

Solving variant ambiguity by specifying a concrete implementation.
  $ dune build --root variant-with-concrete-impl
  Entering directory 'variant-with-concrete-impl'
           bar alias default
  hello from lib2.default

Don't fail when the same library is defined in multiple scopes.
  $ dune build --root same-lib-in-multiple-scopes
  Entering directory 'same-lib-in-multiple-scopes'
