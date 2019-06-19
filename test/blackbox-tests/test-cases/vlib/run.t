virtual libraries may not implement their virtual modules

  $ dune build --root invalid-virtual-lib
  Entering directory 'invalid-virtual-lib'
  File "dune", line 3, characters 18-21:
  3 |  (virtual_modules foo bar))
                        ^^^
  Error: The following modules have an implementation, they cannot be listed as virtual:
  - Foo
  [1]

  $ cd module-fields && ocaml test.ml
  impl: true. modules_without_implementation: true. virtual_modules: true. private_modules: true
  File "dune", line 4, characters 18-19:
  4 |  (virtual_modules m)
                        ^
  Error: The following modules are declared as virtual and private:
  - M
  This is not possible.
  -------------------------
  impl: true. modules_without_implementation: true. virtual_modules: false. private_modules: true
  File "dune", line 3, characters 33-34:
  3 |  (modules_without_implementation m)
                                       ^
  Error: The following modules have an implementation, they cannot be listed as modules_without_implementation:
  - M
  -------------------------
  impl: true. modules_without_implementation: false. virtual_modules: true. private_modules: true
  File "dune", line 3, characters 18-19:
  3 |  (virtual_modules m)
                        ^
  Error: The following modules are declared as virtual and private:
  - M
  This is not possible.
  -------------------------
  impl: true. modules_without_implementation: false. virtual_modules: false. private_modules: true
  -------------------------
  impl: false. modules_without_implementation: true. virtual_modules: true. private_modules: true
  File "dune", line 4, characters 18-19:
  4 |  (virtual_modules m)
                        ^
  Error: The following modules are declared as virtual and private:
  - M
  This is not possible.
  -------------------------
  impl: false. modules_without_implementation: true. virtual_modules: false. private_modules: true
  -------------------------
  impl: false. modules_without_implementation: false. virtual_modules: true. private_modules: true
  File "dune", line 3, characters 18-19:
  3 |  (virtual_modules m)
                        ^
  Error: The following modules are declared as virtual and private:
  - M
  This is not possible.
  -------------------------
  impl: false. modules_without_implementation: false. virtual_modules: false. private_modules: true
  File "dune", line 1, characters 0-42:
  1 | (library
  2 |  (name foo)
  3 |  (private_modules m))
  Warning: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation m)
  
  This will become an error in the future.
  -------------------------
  impl: true. modules_without_implementation: true. virtual_modules: true. private_modules: false
  File "dune", line 4, characters 18-19:
  4 |  (virtual_modules m))
                        ^
  Error: These modules appear in the virtual_libraries and modules_without_implementation fields:
  - M
  This is not possible.
  -------------------------
  impl: true. modules_without_implementation: true. virtual_modules: false. private_modules: false
  File "dune", line 3, characters 33-34:
  3 |  (modules_without_implementation m))
                                       ^
  Error: The following modules have an implementation, they cannot be listed as modules_without_implementation:
  - M
  -------------------------
  impl: true. modules_without_implementation: false. virtual_modules: true. private_modules: false
  File "dune", line 3, characters 18-19:
  3 |  (virtual_modules m))
                        ^
  Error: The following modules have an implementation, they cannot be listed as virtual:
  - M
  -------------------------
  impl: true. modules_without_implementation: false. virtual_modules: false. private_modules: false
  -------------------------
  impl: false. modules_without_implementation: true. virtual_modules: true. private_modules: false
  File "dune", line 4, characters 18-19:
  4 |  (virtual_modules m))
                        ^
  Error: These modules appear in the virtual_libraries and modules_without_implementation fields:
  - M
  This is not possible.
  -------------------------
  impl: false. modules_without_implementation: true. virtual_modules: false. private_modules: false
  -------------------------
  impl: false. modules_without_implementation: false. virtual_modules: true. private_modules: false
  -------------------------
  impl: false. modules_without_implementation: false. virtual_modules: false. private_modules: false
  File "dune", line 1, characters 0-21:
  1 | (library
  2 |  (name foo))
  Warning: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation m)
  
  This will become an error in the future.
  -------------------------

Implementations cannot introduce new modules to the library's interface
  $ dune build --root impl-public-modules
  Entering directory 'impl-public-modules'
  Error: Implementations of wrapped libraries cannot introduce new public modules.
  The following modules:
  - Baz
   must all be marked as private using the (private_modules ..) field.
  [1]

They can only introduce private modules:
  $ dune build --root impl-private-modules
  Entering directory 'impl-private-modules'
          test alias default
  Private module Baz
  implementing bar

Virtual library with a single module
  $ dune build --root variants-simple
  Entering directory 'variants-simple'
           foo alias default
  running implementation

Virtual library where a wrapped module is virtual
  $ dune build --root variants-sub-module
  Entering directory 'variants-sub-module'
           run alias default
  Impl's Vmd.run ()

Executable that tries to build against a virtual library without an implementation
  $ dune build --root missing-implementation
  Entering directory 'missing-implementation'
  Error: No implementation found for virtual library "vlib" in
  _build/default/vlib.
  -> required by executable foo in dune:2
  [1]

Executable that tries to use two implementations for the same virtual lib
  $ dune build --root double-implementation
  Entering directory 'double-implementation'
  Error: Conflicting implementations for virtual library "vlib" in
  _build/default/vlib:
  - "impl1" in _build/default/impl1
    -> required by library "bar" in _build/default
  - "impl2" in _build/default/impl2
  This cannot work.
  -> required by executable foo in dune:2
  [1]

Install files for implemenations and virtual libs have all the artifacts:
  $ dune build --root install-file
  Entering directory 'install-file'
  lib: [
    "_build/install/default/lib/vlib/META"
    "_build/install/default/lib/vlib/dune-package"
    "_build/install/default/lib/vlib/foo.mli"
    "_build/install/default/lib/vlib/opam"
    "_build/install/default/lib/vlib/vlib.cmi"
    "_build/install/default/lib/vlib/vlib.cmo"
    "_build/install/default/lib/vlib/vlib.cmt"
    "_build/install/default/lib/vlib/vlib.cmx"
    "_build/install/default/lib/vlib/vlib.ml"
    "_build/install/default/lib/vlib/vlib$ext_obj"
    "_build/install/default/lib/vlib/vlib__Foo.cmi"
    "_build/install/default/lib/vlib/vlib__Foo.cmti"
  ]
  lib: [
    "_build/install/default/lib/impl/META"
    "_build/install/default/lib/impl/dune-package"
    "_build/install/default/lib/impl/foo.ml"
    "_build/install/default/lib/impl/impl$ext_lib"
    "_build/install/default/lib/impl/impl.cma"
    "_build/install/default/lib/impl/impl.cmxa"
    "_build/install/default/lib/impl/impl.cmxs"
    "_build/install/default/lib/impl/opam"
    "_build/install/default/lib/impl/vlib__Foo.cmi"
    "_build/install/default/lib/impl/vlib__Foo.cmt"
    "_build/install/default/lib/impl/vlib__Foo.cmx"
    "_build/install/default/lib/impl/vlib__impl__.cmi"
    "_build/install/default/lib/impl/vlib__impl__.cmt"
    "_build/install/default/lib/impl/vlib__impl__.cmx"
    "_build/install/default/lib/impl/vlib__impl__.ml"
  ]

Implementations may refer to virtual library's modules
  $ dune build --root impl-using-vlib-modules
  Entering directory 'impl-using-vlib-modules'
          test alias default
  bar from vlib
  Foo.run implemented

Implementations may have private modules that have overlapping names with the
virtual lib
  $ dune build --root private-modules-overlapping-names
  Entering directory 'private-modules-overlapping-names'
           foo alias default
  impl's own Priv.run
  implementation of foo

Unwrapped virtual library
  $ dune build --root unwrapped
  Entering directory 'unwrapped'
           foo alias default
  Running from vlib_more
  running implementation

Unwrapped virtual library
  $ dune build @install --root unwrapped/vlib
  Entering directory 'unwrapped/vlib'
  $ env OCAMLPATH=unwrapped/vlib/_build/install/default/lib dune build --root unwrapped/impl --debug-dependency-path
  Entering directory 'unwrapped/impl'
           foo alias default
  Running from vlib_more
  running implementation

Implementations may not provide a library interface module unless it is virtual.
There should be an error message that clarifies this.
  $ dune build --root impl-lib-interface-module @all
  Entering directory 'impl-lib-interface-module'
  Error: Implementations of wrapped libraries cannot introduce new public modules.
  The following modules:
  - Vlib
   must all be marked as private using the (private_modules ..) field.
  [1]

Test that implementing vlibs that aren't present is impossible
  $ dune build --root no-vlib-present
  Entering directory 'no-vlib-present'
  File "dune", line 3, characters 13-27:
  3 |  (implements foobar12312414))
                   ^^^^^^^^^^^^^^
  Error: Library "foobar12312414" not found.
  Hint: try: dune external-lib-deps --missing --root no-vlib-present @@default
  [1]

Test that trying to implement libraries that aren't virtual results in an
appropriate error message.
  $ dune build --root impl-not-virtual
  Entering directory 'impl-not-virtual'
  File "impl/dune", line 3, characters 13-16:
  3 |  (implements lib))
                   ^^^
  Error: Library "lib" is not virtual. It cannot be implemented by "impl".
  [1]

Test that trying to implement external libraries that aren't virtual results in
an appropriate error message.
  $ dune build --root impl-not-virtual-external
  Entering directory 'impl-not-virtual-external'
  File "dune", line 7, characters 13-30:
  7 |  (implements dune.configurator))
                   ^^^^^^^^^^^^^^^^^
  Error: Library "dune.configurator" is not virtual. It cannot be implemented
  by "foobar".
  [1]

Test that we can implement external libraries.

First we create an external library
  $ dune build --root implements-external/vlib @install
  Entering directory 'implements-external/vlib'

Then we make sure that we can implement it
  $ env OCAMLPATH=implements-external/vlib/_build/install/default/lib dune build --root implements-external/impl --debug-dependency-path
  Entering directory 'implements-external/impl'
          test alias default
  bar from vlib
  Foo.run implemented

Make sure that we can also implement native only variants
  $ env OCAMLPATH=implements-external/vlib/_build/install/default/lib dune build --root implements-external/impl-native-only --debug-dependency-path
  Entering directory 'implements-external/impl-native-only'
           run alias default
  implement virtual module

We can implement external variants with mli only modules
  $ env OCAMLPATH=implements-external/vlib/_build/install/default/lib dune build --root implements-external/impl-intf-only --debug-dependency-path
  Entering directory 'implements-external/impl-intf-only'
           run alias default
  implemented mli only
  magic number: 42

Implement external virtual libraries with private modules
  $ env OCAMLPATH=implements-external/vlib/_build/install/default/lib dune build --root implements-external/impl-private-module --debug-dependency-path
  Entering directory 'implements-external/impl-private-module'
           run alias default
  Name: implement virtual module. Magic number: 42

Include variants and implementation information in dune-package
  $ dune build --root dune-package-info
  Entering directory 'dune-package-info'
  (lang dune 1.11)
  (name foo)
  (library
   (name foo.impl)
   (kind normal)
   (archives (byte impl/impl.cma) (native impl/impl.cmxa))
   (plugins (byte impl/impl.cma) (native impl/impl.cmxs))
   (foreign_archives (native impl/impl$ext_lib))
   (requires foo.vlib)
   (implements vlib)
   (main_module_name Vlib)
   (modes byte native)
   (modules
    (alias_module
     (name Vlib__impl__)
     (obj_name vlib__impl__)
     (visibility public)
     (kind alias)
     (impl))
    (main_module_name Vlib)
    (modules
     ((name Vmod)
      (obj_name vlib__Vmod)
      (visibility public)
      (kind impl_vmodule)
      (impl)))
    (wrapped true)))
  (library
   (name foo.vlib)
   (kind normal)
   (virtual)
   (foreign_archives (native vlib/vlib$ext_lib))
   (main_module_name Vlib)
   (modes byte native)
   (modules
    (alias_module
     (name Vlib)
     (obj_name vlib)
     (visibility public)
     (kind alias)
     (impl))
    (main_module_name Vlib)
    (modules
     ((name Vmod)
      (obj_name vlib__Vmod)
      (visibility public)
      (kind virtual)
      (intf)))
    (wrapped true)))

Virtual libraries and preprocessed source
  $ dune build --root preprocess
  Entering directory 'preprocess'
          test alias default
  foo
