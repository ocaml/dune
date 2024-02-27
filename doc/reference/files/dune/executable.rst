############
 executable
############

The ``executable`` stanza must be used to describe an executable. The
format of executable stanzas is as follows:

.. code:: dune

   (executable
    (name <name>)
    <optional-fields>)

``<name>`` is a module name that contains the executable's main entry
point. There can be additional modules in the current directory; you
only need to specify the entry point. Given an ``executable`` stanza
with ``(name <name>)``, Dune will know how to build ``<name>.exe``. If
requested, it will also know how to build ``<name>.bc`` and
``<name>.bc.js`` (Dune 2.0 and up also need specific configuration (see
the ``modes`` optional field below)).

``<name>.exe`` is a native code executable, ``<name>.bc`` is a bytecode
executable which requires ``ocamlrun`` to run, and ``<name>.bc.js`` is a
JavaScript generated using ``js_of_ocaml``.

Please note: in case native compilation is not available, ``<name>.exe``
will be a custom bytecode executable, in the sense of ``ocamlc
-custom``. This means it's a native executable that embeds the
``ocamlrun`` virtual machine as well as the bytecode, so you can always
rely on ``<name>.exe`` being available. Moreover, it is usually
preferable to use ``<name>.exe`` in custom rules or when calling the
executable by hand because running a bytecode executable often requires
loading shared libraries that are locally built. This requires
additional setup, such as setting specific environment variables, which
Dune doesn't do at the moment.

Native compilation isn't available when there is no ``ocamlopt`` binary
at the same place as ``ocamlc`` was found.

Executables can also be linked as object or shared object files. See
`linking modes`_ for more information.

Starting from Dune 3.0, it's possible to automatically generate empty
interface files for executables. See
:doc:`/reference/files/dune-project/executables_implicit_empty_intf`.

``<optional-fields>`` are:

-  ``(public_name <public-name>)`` specifies that the executable should
   be installed under this name. It's the same as adding the following
   stanza to your ``dune`` file:

      .. code:: dune

         (install
          (section bin)
          (files (<name>.exe as <public-name>)))

   As a special case, ``(public_name -)`` is the same as if the field
   was absent.

.. _shared-exe-fields:

-  ``(package <package>)`` if there is a ``(public_name ...)`` field,
   this specifies the package the executables are part of it.

-  ``(libraries <library-dependencies>)`` specifies the library
   dependencies. See :doc:`/reference/library-dependencies` for more
   details.

-  ``(link_flags <flags>)`` specifies additional flags to pass to the
   linker. This field supports ``(:include ...)`` forms.

-  ``(link_deps (<deps-conf list>))`` specifies the dependencies used
   only by the linker, i.e., when using a version script. See
   :doc:`/concepts/dependency-spec` for more details.

-  ``(modules <modules>)`` specifies which modules in the current
   directory Dune should consider when building this executable. Modules
   not listed here will be ignored and cannot be used inside the
   executable described by the current stanza. It is interpreted in the
   same way as the ``(modules ...)`` field of :doc:`library`.

-  ``(root_module <module>)`` specifies a ``root_module`` that collects
   all listed dependencies in ``libraries``. See the documentation for
   ``root_module`` in the library stanza.

-  ``(modes (<modes>))`` sets the `linking modes`_. The default is
   ``(exe)``. Before Dune 2.0, it formerly was ``(byte exe)``.

-  ``(preprocess <preprocess-spec>)`` is the same as the ``(preprocess
   ...)`` field of :doc:`library`.

   ``(preprocessor_deps ...)`` field of :doc:`library`.

-  ``(preprocessor_deps (<deps-conf list>))`` is the same as the

-  ``js_of_ocaml``: See the section about :ref:`jsoo-field`

-  ``flags``, ``ocamlc_flags``, and ``ocamlopt_flags``: See
   :doc:`/concepts/ocaml-flags`.

-  ``(modules_without_implementation <modules>)`` is the same as the
   corresponding field of :doc:`library`.

-  ``(allow_overlapping_dependencies)`` is the same as the corresponding
   field of :doc:`library`.

-  ``(optional)`` is the same as the corresponding field of
   :doc:`library`.

-  ``(enabled_if <blang expression>)`` is the same as the corresponding
   field of :doc:`library`.

-  ``(promote <options>)`` allows promoting the linked executables to
   the source tree. The options are the same as for the :ref:`rule
   promote mode <promote>`. Adding ``(promote (until-clean))`` to an
   ``executable`` stanza will cause Dune to copy the ``.exe`` files to
   the source tree and use ``dune clean`` to delete them.

-  ``(foreign_stubs <foreign-stubs-spec>)`` specifies foreign source
   files, e.g., C or C++ stubs, to be linked into the executable. See
   :doc:`/reference/foreign` for more details.

-  ``(foreign_archives <foreign-archives-list>)`` specifies archives of
   foreign object files to be linked into the executable. See the
   section :ref:`foreign-archives` for more details.

-  ``(forbidden_libraries <libraries>)`` ensures that the given
   libraries are not linked in the resulting executable. If they end up
   being pulled in, either through a direct or transitive dependency,
   Dune fails with an error message explaining how the library was
   pulled in. This field has been available since Dune 2.0.

-  ``(embed_in_plugin_libraries <library-list>)`` specifies a list of
   libraries to link statically when using the ``plugin`` linking mode.
   By default, no libraries are linked in. Note that you may need to
   also use the ``-linkall`` flag if some of the libraries listed here
   are not referenced from any of the plugin modules.

-  ``(ctypes <ctypes field>)`` instructs Dune to use ctypes stubgen to
   process your type and function descriptions for binding system
   libraries, vendored libraries, or other foreign code. See
   :ref:`ctypes-stubgen` for a full reference. This field is available
   since the 3.0 version of the Dune language.

-  ``(empty_module_interface_if_absent)`` causes the generation of empty
   interfaces for every module that does not have an interface file
   already. Useful when modules are used solely for their side-effects.
   This field is available since the 3.0 version of the Dune language.

***************
 Linking Modes
***************

The ``modes`` field allows selecting which linking modes will be used to
link executables. Each mode is a pair ``(<compilation-mode>
<binary-kind>)``, where ``<compilation-mode>`` describes whether the
bytecode or native code backend of the OCaml compiler should be used and
``<binary-kind>`` describes what kind of file should be produced.

``<compilation-mode>`` must be ``byte``, ``native``, or ``best``, where
``best`` is ``native`` with a fallback to bytecode when native
compilation isn't available.

``<binary-kind>`` is one of:

-  ``c`` for producing OCaml bytecode embedded in a C file

-  ``exe`` for normal executables

-  ``object`` for producing static object files that can be manually
   linked into C applications

-  ``shared_object`` for producing object files that can be dynamically
   loaded into an application. This mode can be used to write a plugin
   in OCaml for a non-OCaml application.

-  ``js`` for producing JavaScript from bytecode executables, see
   :doc:`/reference/files/dune-project/explicit_js_mode`.

-  ``plugin`` for producing a plugin (``.cmxs`` if native or ``.cma`` if
   bytecode).

For instance the following ``executables`` stanza will produce bytecode
executables and native shared objects:

.. code:: dune

   (executables
     (names a b c)
     (modes (byte exe) (native shared_object)))

Additionally, you can use the following shorthands:

-  ``c`` for ``(byte c)``
-  ``exe`` for ``(best exe)``
-  ``object`` for ``(best object)``
-  ``shared_object`` for ``(best shared_object)``
-  ``byte`` for ``(byte exe)``
-  ``native`` for ``(native exe)``
-  ``js`` for ``(byte js)``
-  ``plugin`` for ``(best plugin)``

For instance, the following ``modes`` fields are all equivalent:

.. code:: dune

   (modes (exe object shared_object))
   (modes ((best exe)
           (best object)
           (best shared_object)))

Lastly, use the special mode ``byte_complete`` for building a bytecode
executable as a native self-contained executable, i.e., an executable
that doesn't require the ``ocamlrun`` program to run and doesn't require
the C stubs to be installed as shared object files.

The extensions for the various linking modes are chosen as follows:

..
   =========================== =================

..
   linking mode                extensions

..
   --------------------------- -----------------

..
   byte                        .bc

..
   native/best                 .exe

..
   byte_complete               .bc.exe

..
   (byte object)               .bc%{ext_obj}

..
   (native/best object)        .exe%{ext_obj}

..
   (byte shared_object)        .bc%{ext_dll}

..
   (native/best shared_object) %{ext_dll}

..
   c                           .bc.c

..
   js                          .bc.js

..
   (best plugin)               %{ext_plugin}

..
   (byte plugin)               .cma

..
   (native plugin)             .cmxs

..
   =========================== =================

``%{ext_obj}`` and ``%{ext_dll}`` are the extensions for object and
shared object files. Their value depends on the OS. For instance, on
Unix ``%{ext_obj}`` is usually ``.o`` and ``%{ext_dll}`` is usually
``.so``, while on Windows ``%{ext_obj}`` is ``.obj`` and ``%{ext_dll}``
is ``.dll``.

Up to version 3.0 of the Dune language, when ``byte`` is specified but
none of ``native``, ``exe``, or ``byte_complete`` are specified, Dune
implicitly adds a linking mode that's the same as ``byte_complete``, but
it uses the extension ``.exe``. ``.bc`` files require additional files
at runtime that aren't currently tracked by Dune, so they don't run
``.bc`` files during the build. Run the ``.bc.exe`` or ``.exe`` ones
instead, as these are self-contained.

Lastly, note that ``.bc`` executables cannot contain C stubs. If your
executable contains C stubs you may want to use ``(modes exe)``.

.. _jsoo-field:

*************
 js_of_ocaml
*************

In ``library`` and ``executables`` stanzas, you can specify
``js_of_ocaml`` options using ``(js_of_ocaml (<js_of_ocaml-options>))``.

``<js_of_ocaml-options>`` are all optional:

-  ``(flags <flags>)`` to specify flags passed to ``js_of_ocaml
   compile``. This field supports ``(:include ...)`` forms

-  ``(build_runtime_flags <flags>)`` to specify flags passed to
   ``js_of_ocaml build-runtime``. This field supports ``(:include ...)``
   forms

-  ``(link_flags <flags>)`` to specify flags passed to ``js_of_ocaml
   link``. This field supports ``(:include ...)`` forms

-  ``(javascript_files (<files-list>))`` to specify ``js_of_ocaml``
   JavaScript runtime files.

``<flags>`` is specified in the :doc:`/reference/ordered-set-language`.

The default value for ``(flags ...)`` depends on the selected build
profile. The build profile ``dev`` (the default) will enable sourcemap
and the pretty JavaScript output.

See :ref:`jsoo` for more information.

#############
 executables
#############

There is a very subtle difference in the naming of these stanzas. One is
``executables``, plural, and the other is ``executable``, singular. The
``executables`` stanza is very similar as the ``executable`` stanza but
can be used to to describe several executables sharing the same
configuration, so the plural ``executables`` stanza is used to describe
more than one executable.

It shares the same fields as the ``executable`` stanza, except that
instead of ``(name ...)`` and ``(public_name ...)`` you must use the
plural versions as well:

-  ``(names <names>)`` where ``<names>`` is a list of entry point names.
   Compare with ``executable``, where you only need to specify the
   modules containing the entry point of each executable.

-  ``(public_names <names>)`` describes under what name to install each
   executable. The list of names must be of the same length as the list
   in the ``(names ...)`` field. Moreover, you can use ``-`` for
   executables that shouldn't be installed.

However, using ``executables`` the executables defined in the stanza are
allowed to share modules.

Given modules ``Foo``, ``Bar`` and ``Baz`` the usage of ``executables``
can simplify the code:

.. code:: dune

   (executables
     (names foo bar))

Instead of the more complex

.. code:: dune

   (library
     (name baz)
     (modules baz))

   (executable
     (name foo)
     (modules foo)
     (libraries baz))

   (executable
     (name bar)
     (modules bar)
     (libraries baz))
