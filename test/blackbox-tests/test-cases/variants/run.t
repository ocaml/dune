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
  File "dune", line 4, characters 11-14:
  4 |  (variants a b))
                 ^^^
  Error: Multiple solutions for the implementation of "vlib" with variants a
  and b:
  - "lib_b" in _build/default/lib.b (variant b)
  - "lib2_a" in _build/default/lib2$ext_lib (variant a)
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
  File "a/dune", line 1, characters 0-71:
  1 | (library
  2 |  (public_name a)
  3 |  (implements b)
  4 |  (variant test)
  5 |  (modules x))
  Error: No rule found for b/.b.objs/b.ml-gen.all-deps
  [1]
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
   (main_module_name B)
   (modes byte native)
   (modules
    wrapped
    (alias_module
     (name B__a__)
     (obj_name b__a__)
     (visibility public)
     (kind alias)
     (impl))
    (main_module_name B__a__)
    (modules
     ((name X) (obj_name b__X) (visibility public) (kind impl_vmodule) (impl)))))
  $ cat  dune-package/_build/install/default/lib/b/dune-package
  (lang dune 1.11)
  (name b)
  (library
   (name b)
   (kind normal)
   (virtual)
   (foreign_archives (native b$ext_lib))
   (known_implementations (test a))
   (main_module_name B)
   (modes byte native)
   (modules
    wrapped
    (alias_module (name B) (obj_name b) (visibility public) (kind alias) (impl))
    (main_module_name B)
    (modules
     ((name X) (obj_name b__X) (visibility public) (kind virtual) (intf)))))

Test variants for an external library

First we create an external library and implementation
  $ dune build --root external/lib @install
  Entering directory 'external/lib'

Then we make sure that it works fine.
  $ env OCAMLPATH=external/lib/_build/install/default/lib dune build --root external/exe --debug-dependency-path
  Entering directory 'external/exe'
      ocamlopt bar.exe (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.07.1/bin/ocamlopt.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-66-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -o bar.exe -I $TESTCASE_ROOT/external/lib/_build/install/default/lib/vlib .bar.eobjs/native/bar.cmx)
  File "_none_", line 1:
  Error: No implementations provided for the following modules:
           X referenced from .bar.eobjs/native/bar.cmx
  -> required by bar.exe
  -> required by alias default
  -> required by alias default
  [1]

Variant ambiguity is forbidden even if a concrete implementation is provided.
  $ dune build --root variant-with-concrete-impl
  Entering directory 'variant-with-concrete-impl'
  Error: Two implementations of vlib have the same variant "default":
  - lib2_default (lib2.default/dune:1)
  - lib_default (lib.default/dune:1)
  [1]

Don't fail when the same library is defined in multiple scopes.
  $ dune build --root same-lib-in-multiple-scopes
  Entering directory 'same-lib-in-multiple-scopes'
  File "test/unix/dune", line 1, characters 0-91:
  1 | (library
  2 |   (name test_unix)
  3 |   (public_name test-unix)
  4 |   (implements test)
  5 |   (variant unix))
  Error: No rule found for test/virt/.test.objs/test.ml-gen.all-deps
  [1]
