******************************
Dealing with Foreign Libraries
******************************

The OCaml programming language can interface with libraries written in foreign
languages such as C. This section explains how to do this with Dune. Note that
it does not cover how to write the C stubs themselves, but this is covered by
the `OCaml manual <https://caml.inria.fr/pub/docs/manual-ocaml/intfc.html>`_.

More precisely, this section covers:

- How to add C/C++ stubs to an OCaml library
- How to pass specific compilation flags for compiling the stubs
- How to build a library with a foreign build system

In general, Dune has limited support for building source files written in
foreign languages. This support is suitable for most OCaml projects containing
C stubs, but it is too limited for building complex libraries written in C or
other languages. For such cases, Dune can integrate a foreign build system into
a normal Dune build.

Adding C/C++ Stubs to an OCaml Library
======================================

To add C stubs to an OCaml library, simply list the C files without the ``.c``
extension in the :ref:`foreign-stubs` field. For instance:

.. code:: scheme

   (library
    (name mylib)
    (foreign_stubs (language c) (names file1 file2)))

You can also add C++ stubs to an OCaml library by specifying
``(language cxx)`` instead.

Dune is currently not flexible regarding the extension of the C/C++ source
files. They have to be ``.c`` for C files and ``.cpp``, ``.cc`` or ``.cxx`` for
C++ files. If you have source files with other extensions and you want to build
them with Dune, you need to rename them first. Alternatively, you can use the
:ref:`foreign build sandboxing <foreign-sandboxing>` method described below.

Header Files
------------

C/C++ source files may include header files in the same directory as the C/C++
source files or in the same directory group when using :ref:`include_subdirs`.

The header files must have the ``.h`` extension.

Installing Header Files
-----------------------

It is sometimes desirable to install header files with the library. For that
you have two choices: install them explicitly with an :ref:`install` stanza or
use the ``install_c_headers`` field of the :ref:`library` stanza. This field
takes a list of header files names without the ``.h`` extension. When a library
installs header files, they are made visible to users of the library via the
include search path.

.. _ctypes-stubgen:

Stub Generation with Dune Ctypes
================================

Beginning in Dune 3.0, it's possible to use the ctypes_ stanza to generate
bindings for C libraries without writing any C code.

Note that Dune support for this feature is experimental and is not subject to
backward compatibility guarantees.

To use Dune ctypes stub generation, you must provide two OCaml modules: a "type
description" module for describing the C library types and constants, and a
"function description" module for describing the C library functions.
Additionally, you must list any C headers and a method for resolving build and
link flags.

If you're binding a library distributed by your OS, you can use the pkg-config_
utility to resolve any build and link flags. Alternatively, if you're using a
locally installed library or a vendored library, you can provide the flags
manually.

The "type description" module must define a functor named ``Types`` with
signature ``Ctypes.TYPE``. The "function description" module must define a
functor named ``Functions`` with signature ``Ctypes.FOREIGN``.

A Toy Example
-------------

To begin, you must declare the ``ctypes`` extension in your ``dune-project``
file:

.. code:: scheme

  (lang dune 3.7)
  (using ctypes 0.1)


Next, here is a ``dune`` file you can use to define an OCaml program that binds
a C system library called ``libfoo``, which offers ``foo.h`` in a standard
location.

.. code:: scheme

   (executable
    (name foo)
    (libraries core)
    ; ctypes backward compatibility shims warn sometimes; suppress them
    (flags (:standard -w -9-27))
    (ctypes
     (external_library_name libfoo)
     (build_flags_resolver pkg_config)
     (headers (include "foo.h"))
     (type_description
      (instance Type)
      (functor Type_description))
     (function_description
      (concurrency unlocked)
      (instance Function)
      (functor Function_description))
     (generated_types Types_generated)
     (generated_entry_point C)))

This stanza will introduce a module named ``C`` into your project, with the
sub-modules ``Types`` and ``Functions`` that will have your fully-bound C
types, constants, and functions.

Given ``libfoo`` with the C header file ``foo.h``:

.. code:: c

  #define FOO_VERSION 1

  int foo_init(void);

  int foo_fnubar(char *);

  void foo_exit(void);

Your example ``type_description.ml`` file is:

.. code:: ocaml

  open Ctypes

  module Types (F : Ctypes.TYPE) = struct
    open F

    let foo_version = constant "FOO_VERSION" int
  end

Your example ``function_description.ml`` file is:

.. code:: ocaml

  open Ctypes

  (* This Types_generated module is an instantiation of the Types
     functor defined in the type_description.ml file. It's generated by
     a C program that Dune creates and runs behind the scenes. *)
  module Types = Types_generated

  module Functions (F : Ctypes.FOREIGN) = struct
    open F

    let foo_init = foreign "foo_init" (void @-> returning int)

    let foo_fnubar = foreign "foo_fnubar" (string_opt @-> returning int)

    let foo_exit = foreign "foo_exit" (void @-> returning void)
  end

Finally, the entry point of your executable named above, ``foo.ml``,
demonstrates how to access the bound C library functions and values:

.. code:: ocaml

  let () =
    if (C.Types.foo_version <> 1) then
      failwith "foo only works with libfoo version 1";

    match C.Functions.foo_init () with
    | 0 ->
      C.Functions.foo_fnubar "fnubar!";
      C.Functions.foo_exit ()
    | err_code ->
      Printf.eprintf "foo_init failed: %d" err_code;
  ;;

From here, one only needs to run ``dune build ./foo.exe`` to generate the stubs
and build and link the example ``foo.exe`` program.

Complete information about the ``ctypes`` combinators used above is available
at the ctypes_ project.

Ctypes Stanza Reference
------------------------

The ``ctypes`` stanza can be used in any ``executable(s)`` or ``library``
stanza.

.. code:: scheme

  ((executable|library)
    ...
    (ctypes
      (external_library_name <package-name>)
      (type_description
        (instance <module-name>)
        (functor <module-name>))
      (function_description
        (instance <module-name>)
        (functor <module-name>)
        <optional-function-description-fields>)
      (generated_entry_point <module-name>)
      <optional-ctypes-fields>)
    )

- ``type_description``: the ``functor`` module is a description of the C
  library types and constants written in the ``ctypes`` domain-specific
  language you wish to bind. The ``instance`` module is the name of the
  instantiated functor, inserted into the top-level of the
  ``generated_entry_point`` module.

- ``function_description``: the ``functor`` module is a description of the C
  library functions written in the ``ctypes`` domain-specific language you wish
  to bind. The ``instance`` module is the name of the instantiated functor,
  inserted into the top-level of the ``generated_entry_point`` module. The
  ``function_description`` stanza can be repeated. This is useful if you need
  to specify sets of functions with different concurrency policies (see below).

The instantiated types described above can be accessed from the function
descriptions by referencing them as the module specified in optional
``generated_types`` field.

``<optional-ctypes-fields>`` are:

- ``(build_flags_resolver <pkg_config|vendored-stanza>)`` tells Dune how to
  compile and link your foreign library. Specifying ``pkg_config`` will use
  the pkg-config_ tool to query the compilation and link flags for
  ``external_library_name``. For vendored libraries, provide the build and link
  flags using ``vendored`` stanza. If ``build_flags_resolver`` is not
  specified, the default of ``pkg_config`` will be used.

- ``(generated_types <module-name>)`` is the name of an intermediate module. By
  default, it's named ``Types_generated``. You can use this module to access
  the types defined in ``Type_description`` from your ``Function_description``
  module(s).

- ``(generated_entry_point <module-name>)`` is the name of a generated module
  that your instantiated ``Types`` and ``Function`` modules will instantiated
  under. We suggest calling it ``C``.

- Headers can be added to the generated C files:
   - ``(headers (include "include1" "include2" ...))`` adds ``#include
     <include1>``, ``#include <include2>``. It uses the :ref:`ordered-set-language`.
   - ``(headers (preamble <preamble>)`` adds directly the preamble. Variables
     can be used in ``<preamble>`` such as ``%{read: }``.

- Since the Dune's ``ctypes`` feature is still experimental, it could be useful to
  add additional dependencies in order to make sure that local
  headers or libraries are available: ``(deps <deps-conf list>)``. See the
  :ref:`deps-field` section for more details.

``<optional-function-description-fields>`` are:

- ``(concurrency <sequential|unlocked|lwt_jobs|lwt_preemptive>)`` tells ``ctypes
  stubgen`` whether to call your C functions with the runtime lock held or
  released. These correspond to the ``concurrency_policy`` type in the
  ``ctypes`` library. If ``concurrency`` is not specified, the default of
  ``sequential`` will be used.

- ``(errno_policy <ignore_errno|return_errno>)`` specifies the errno_policy_
  passed to the code generator. With ``ignore_errno``, the errno variable is
  not accessed or returned by function calls. With ``return_errno``, all
  functions will return the tuple ``(retval, errno)``.

``<vendored-stanza>`` is:

- ``(vendored (c_flags <flags>) (c_library_flags <flags>))`` provide the build
  and link flags for binding your vendored code. You must also provide
  instructions in your ``dune`` file on how to build the vendored foreign
  library; see the :ref:`foreign_library` stanza. Usually the ``<flags>`` should
  contain ``:standard`` in order to add the default flags used by the OCaml
  compiler for C files :ref:`always-add-cflags`.


.. _foreign-sandboxing:

Foreign Build Sandboxing
========================

When the build of a C library is too complicated to express in the
Dune language, it's possible to simply *sandbox* a foreign
build. Note that this method can be used to build other things, not
just C libraries.

To do that, follow the following procedure:

- Put all the foreign code in a sub-directory
- Tell Dune not to interpret configuration files in this directory via an
  :ref:`data_only_dirs <dune-data_only_dirs>` stanza
- Write a custom rule that:

  - depends on this directory recursively via :ref:`source_tree <source_tree>`
  - invokes the external build system
  - copies the generated files
  - the C archive ``.a`` must be built with ``-fpic``
  - the ``libfoo.so`` must be copied as ``dllfoo.so``, and no ``libfoo.so``
    should appear, otherwise the dynamic linking of the C library will be
    attempted. However, this usually fails because the ``libfoo.so`` isn't available at
    the time of the execution.
- *Attach* the C archive files to an OCaml library via :ref:`foreign-archives`.

For instance, let's assume that you want to build a C library
``libfoo`` using ``libfoo``'s own build system and attach it to an
OCaml library called ``foo``.

The first step is to put the sources of ``libfoo`` in your project,
for instance in ``src/libfoo``. Then tell Dune to consider
``src/libfoo`` as raw data by writing the following in ``src/dune``:

.. code:: scheme

   (data_only_dirs libfoo)

The next step is to setup the rule to build ``libfoo``. For this,
writing the following code ``src/dune``:

.. code:: scheme

   (rule
    (deps (source_tree libfoo))
    (targets libfoo.a dllfoo.so)
    (action
    (no-infer
     (progn
      (chdir libfoo (run make))
      (copy libfoo/libfoo.a libfoo.a)
      (copy libfoo/libfoo.so dllfoo.so)))))

We copy the resulting archive files to the top directory where they can be
declared as ``targets``. The build is done in a ``no-infer`` action because
``libfoo/libfoo.a`` and ``libfoo/libfoo.so`` are dependencies produced by an
external build system.

The last step is to attach these archives to an OCaml library as follows:

.. code:: scheme

   (library
    (name bar)
    (foreign_archives foo))

Then, whenever you use the ``bar`` library, you'll also be able to
use C functions from ``libfoo``.

Limitations
-----------

When using the sandboxing method, the following limitations apply:

- The build of the foreign code will be sequential
- The build of the foreign code won't be incremental

Both these points could be improved. If you're interested in helping make this
happen, please let the Dune team know and someone will guide you.

Real Example
------------

The `re2 project <https://github.com/janestreet/re2>`_ uses this method to
build the ``re2`` C library. You can look at the file ``re2/src/re2_c/dune`` in
this project to see a full working example.

.. _ctypes: https://github.com/ocamllabs/ocaml-ctypes
.. _pkg-config: https://www.freedesktop.org/wiki/Software/pkg-config/
.. _errno_policy: https://ocaml.org/p/ctypes/0.20.1/doc/Cstubs/index.html#type-errno_policy
