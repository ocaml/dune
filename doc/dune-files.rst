****************
Stanza reference
****************

dune-project
============

These files are used to mark the root of projects as well as define project-wide
parameters. These files are required to have a ``lang`` which controls the names
and contents of all configuration files read by Dune. The ``lang`` stanza looks
like:

.. code:: scheme

          (lang dune 1.0)

Additionally, they can contains the following stanzas.

name
----

Sets the name of the project. This is used by :ref:`dune subst <dune-subst>`
and error messages.

.. code:: scheme

    (name <name>)

version
-------

Sets the version of the project:

.. code:: scheme

    (version <version>)

.. _implicit-transitive-deps:

implicit_transitive_deps
------------------------

By default, dune allows transitive dependencies of dependencies to be used
directly when compiling OCaml. However, this setting can be controlled per
project:

.. code:: scheme

    (implicit_transitive_deps <bool>)

Once this setting is enabled, all dependencies that are directly used by a
library or an executable must be directly added in the ``libraries`` field. We
recommend users to experiment with this mode and report any problems.

Starting from dune 2.0, dune enables this mode by default. However, this can
still be turned off using ``(implicit_transitive_deps false)``.

Note that you must use ``threads.posix`` instead of ``threads`` when using this
mode. This is not an important limitation as ``threads.vm`` are deprecated
anyways.

.. _wrapped-executables:

wrapped_executables
-------------------

Executables are made of compilation units whose names may collide with the
compilation units of libraries. To avoid this possibility, dune prefixes these
compilation unit names with ``Dune__exe__``. This is entirely transparent to
users except for when such executables are debugged. In which case the mangled
names will be visible in the debugger.

Starting from dune 1.11, an option is available to turn on/off name mangling for
executables on a per project basis:

.. code:: scheme

    (wrapped_executables <bool>)

Starting from dune 2.0, dune mangles compilation units of executables by
default. However, this can still be turned off using ``(wrapped_executables
false)``

.. _explicit-js-mode:

explicit_js_mode
----------------

Traditionally, Javascript targets were defined for every bytecode executable.
This was not very precise and did not interact well with the ``@all`` alias.

You can opt out of this behaviour by using:

.. code:: scheme

    (explicit_js_mode)

When this mode is enabled, an explicit ``js`` mode needs to be added to the
``(modes ...)`` field of executables in order to trigger Javascript
compilation. Explicit JS targets declared like this will be attached to the
``@all`` alias.

Starting from dune 2.0 this behaviour is the default, and there is no way to
disable it.

.. _dialect:

dialect
-------

A dialect is an alternative frontend to OCaml (such as ReasonML). It is
described by a pair of file extensions, one corresponding to interfaces and one
to implementations.

A dialect can use the standard OCaml syntax or it can specify an action to
convert from a custom syntax to a binary OCaml abstract syntax tree.

Similarly, a dialect can specify a custom formatter to implement the ``@fmt``
alias, see :ref:`formatting-main`.

When not using a custom syntax or formatting action, a dialect is nothing but a
way to specify custom file extensions for OCaml code.

.. code:: scheme

    (dialect
     (name <name>)
     (implementation
      (extension <string>)
      <optional fields>)
     (interface
      (extension <string>)
      <optional fields>))

``<name>`` is the name of the dialect being defined. It must be unique in a
given project.

``(extension <string>)`` specifies the file extension used for this dialect, for
interfaces and implementations. The extension string must not contain any dots,
and be unique in a given project (so that a given extension can be mapped back
to a corresponding dialect).

``<optional fields>`` are:

- ``(preprocess <action>)`` is the action to run to produce a valid OCaml
  abstract syntax tree. It is expected to read the file given in the variable
  named ``input-file`` and output a *binary* abstract syntax tree on its
  standard output. See :ref:`preprocessing-actions` for more information.

  If the field is not present, it is assumed that the corresponding source code
  is already valid OCaml code and can be passed to the OCaml compiler as-is.


- ``(format <action>)`` is the action to run to format source code for this
  dialect. The action is expected to read the file given in the variable named
  ``input-file`` and output the formatted source code on its standard
  output. For more information. See :ref:`formatting-main` for more information.

  If the field is not present, then if ``(preprocess <action>)`` is not present
  (so that the dialect consists of valid OCaml code), then by default the
  dialect will be formatted as any other OCaml code. Otherwise no special
  formatting will be done.

.. _formatting:

formatting
----------

Starting in dune 2.0, :ref:`formatting-main` is automatically enabled. This can be
controlled by using

.. code:: scheme

    (formatting <setting>)

where ``<setting>`` is one of:

- ``disabled``, meaning that automatic formatting is disabled

- ``(enabled_for <languages>)`` can be used to restrict the languages that are
  considered for formatting.

generate_opam_files
-------------------

Dune is able to use metadata specified in the ``dune-project`` file to generate
``.opam`` files, see :ref:`opam-generation`. To enable this integration, add the
following field to the ``dune-project`` file:

.. code:: scheme

   (generate_opam_files true)

Dune uses the following global fields to set the metadata for all packages
defined in the project:

- ``(license <name>)`` - Specifies the license of the project, ideally as an
  identifier from the `SPDX License List <https://spdx.org/licenses/>`__

- ``(authors <authors>)`` - A list of authors

- ``(maintainers <maintainers>)`` - A list of maintainers

- ``(source <source>)`` - where the source is specified two ways:
  ``(github <user/repo>)`` or ``(uri <uri>)``

- ``(bug_reports <url>)`` - Where to report bugs. This defaults to the GitHub
  issue tracker if the source is specified as a GitHub repository

- ``(homepage <url>)`` - The homepage of the project

- ``(documentation <url>)`` - Where the documentation is hosted

package
-------

Package specific information is specified in the ``(package <package>)`` stanza.
It contains the following fields:

- ``(name <string>)`` is the name of the package. This must be specified.

- ``(synopsis <string>)`` is a short package description

- ``(description <string>)`` is a longer package description

- ``(depends <dep-specification>)`` are package dependencies

- ``(conflicts <dep-specification)`` are package conflicts

- ``(depopts <dep-specification)`` are optional package dependencies

- ``(tags <tags>)`` are the list of tags for the package

The list of dependencies ``<dep-specification>`` is modeled after opam's own
language: The syntax is as a list of the following elements:

.. code::

   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   stage := :with_test | :build | :dev

   constr := (<op> <version>)

   logop := or | and

   dep := (name <stage>)
        | (name <constr>)
        | (name (<logop> (<stage> | <constr>)*))

   dep-specification = dep+

dune
====

``dune`` files are the main part of dune. They are used to describe libraries,
executables, tests, and everything dune needs to know about.

The syntax of ``dune`` files is described in :ref:`metadata-format` section.

``dune`` files are composed of stanzas. For instance a typical
``dune`` looks like:

.. code:: scheme

    (library
     (name mylib)
     (libraries base lwt))

    (rule
     (target foo.ml)
     (deps   generator/gen.exe)
     (action (run %{deps} -o %{target})))

The following sections describe the available stanzas and their meaning.

jbuild_version
--------------

Deprecated. This stanza is no longer used and will be removed in the
future.

.. _library:

library
-------

The ``library`` stanza must be used to describe OCaml libraries. The
format of library stanzas is as follows:

.. code:: scheme

    (library
     (name <library-name>)
     <optional-fields>)

``<library-name>`` is the real name of the library. It determines the
names of the archive files generated for the library as well as the
module name under which the library will be available, unless
``(wrapped false)`` is used (see below). It must be a valid OCaml
module name but doesn't need to start with a uppercase letter.

For instance, the modules of a library named ``foo`` will be
available as ``Foo.XXX`` outside of ``foo`` itself. It is however
allowed to write an explicit ``Foo`` module, in which case this will
be the interface of the library and you are free to expose only the
modules you want.

Note that by default libraries and other things that consume
OCaml/Reason modules only consume modules from the directory where the
stanza appear. In order to declare a multi-directory library, you need
to use the :ref:`include_subdirs` stanza.

``<optional-fields>`` are:

- ``(public_name <name>)`` this is the name under which the library can be
  referred to as a dependency when it is not part of the current workspace,
  i.e. when it is installed. Without a ``(public_name ...)`` field, the library
  will not be installed by dune. The public name must start by the package
  name it is part of and optionally followed by a dot and anything else you
  want. The package name must be one of the packages that dune knows about,
  as determined by the :ref:`opam-files`

- ``(synopsis <string>)`` should give a one-line description of the library.
  This is used by tools that list installed libraries

- ``(modules <modules>)`` specifies what modules are part of the library. By
  default dune will use all the .ml/.re files in the same directory as the
  ``dune`` file. This include ones that are present in the file system as well
  as ones generated by user rules. You can restrict this list by using a
  ``(modules <modules>)`` field. ``<modules>`` uses the :ref:`ordered-set-language`
  where elements are module names and don't need to start with a uppercase
  letter. For instance to exclude module ``Foo``: ``(modules (:standard \
  foo))``

- ``(libraries <library-dependencies>)`` is used to specify the dependencies
  of the library. See the section about :ref:`library-deps` for more details

- ``(wrapped <boolean>)`` specifies whether the modules of the library should be
  available only through the top-level library module, or should all be exposed
  at the top level. The default is ``true`` and it is highly recommended to keep
  it this way. Because OCaml top-level modules must all be unique when linking
  an executables, polluting the top-level namespace will make your library
  unusable with other libraries if there is a module name clash. This option is
  only intended for libraries that manually prefix all their modules by the
  library name and to ease porting of existing projects to dune

- ``(wrapped (transition <message>))`` Is the same as ``(wrapped true)`` except
  that it will also generate unwrapped (not prefixed by the library name)
  modules to preserve compatibility. This is useful for libraries that would
  like to transition from ``(wrapped false)`` to ``(wrapped true)`` without
  breaking compatibility for users. The ``<message>`` will be included in the
  deprecation notice for the unwrapped modules.

- ``(preprocess <preprocess-spec>)`` specifies how to preprocess files if
  needed. The default is ``no_preprocessing``. Other options are described in the
  :ref:`preprocessing-spec` section

- ``(preprocessor_deps (<deps-conf list>))`` specifies extra dependencies of the
  preprocessor, for instance if the preprocessor reads a generated file. The
  specification of dependencies is described in the :ref:`deps-field`
  section

- ``(optional)``, if present it indicates that the library should only be built
  and installed if all the dependencies are available, either in the workspace
  or in the installed world. You can use this to provide extra features without
  adding hard dependencies to your project

- ``(c_names (<names>))``, if your library has stubs, you must list the C files
  in this field, without the ``.c`` extension

- ``(cxx_names (<names>))`` is the same as ``c_names`` but for C++ stubs

- ``(install_c_headers (<names>))``, if your library has public C header files
  that must be installed, you must list them in this field, without the ``.h``
  extension

- ``(modes <modes>)`` modes which should be built by default. The
  most common use for this feature is to disable native compilation
  when writing libraries for the OCaml toplevel. The following modes
  are available: ``byte``, ``native`` and ``best``. ``best`` is
  ``native`` or ``byte`` when native compilation is not available

- ``(no_dynlink)`` is to disable dynamic linking of the library. This is for
  advanced use only, by default you shouldn't set this option

- ``(kind <kind>)`` is the kind of the library. The default is ``normal``, other
  available choices are ``ppx_rewriter`` and ``ppx_deriver`` and must be set
  when the library is intended to be used as a ppx rewriter or a ``[@@deriving
  ...]`` plugin. The reason why ``ppx_rewriter`` and ``ppx_deriver`` are split
  is historical and hopefully we won't need two options soon. Both ppx kinds
  support an optional field ``(cookies <cookies>)`` where ``<cookies>`` is a
  list of pairs ``(<name> <value>)`` with ``<name>`` being the cookie name and
  ``<value>`` is a string that supports :ref:`variables` evaluated
  by each invocation of the preprocessor (note: libraries that share
  cookies with the same name should agree on their expanded value)

- ``(ppx_runtime_libraries (<library-names>))`` is for when the library is a ppx
  rewriter or a ``[@@deriving ...]`` plugin and has runtime dependencies. You
  need to specify these runtime dependencies here

- ``(virtual_deps (<opam-packages>)``. Sometimes opam packages enable a specific
  feature only if another package is installed. This is for instance the case of
  ``ctypes`` which will only install ``ctypes.foreign`` if the dummy
  ``ctypes-foreign`` package is installed. You can specify such virtual
  dependencies here. You don't need to do so unless you use dune to
  synthesize the ``depends`` and ``depopts`` sections of your opam file

- ``js_of_ocaml`` sets options for Javascript compilation, see :ref:`jsoo-field`

- ``flags``, ``ocamlc_flags`` and ``ocamlopt_flags``. See the section about
  :ref:`ocaml-flags`

- ``(library_flags (<flags>))`` is a list of flags that are passed as it to
  ``ocamlc`` and ``ocamlopt`` when building the library archive files. You can
  use this to specify ``-linkall`` for instance. ``<flags>`` is a list of
  strings supporting :ref:`variables`

- ``(c_flags <flags>)`` specifies the compilation flags for C stubs, using the
  :ref:`ordered-set-language`. This field supports ``(:include ...)`` forms

- ``(cxx_flags <flags>)`` is the same as ``c_flags`` but for C++ stubs

- ``(c_library_flags <flags>)`` specifies the flags to pass to the C compiler
  when constructing the library archive file for the C stubs. ``<flags>`` uses
  the :ref:`ordered-set-language` and supports ``(:include ...)`` forms. When you
  are writing bindings for a C library named ``bar``, you should typically write
  ``-lbar`` here, or whatever flags are necessary to to link against this
  library

.. _self_build_stubs_archive:

- ``(self_build_stubs_archive <c-libname>)`` indicates to dune that the
  library has stubs, but that the stubs are built manually. The aim of the field
  is to embed a library written in foreign language and/or building with another
  build system. It is not for casual uses, see the `re2 library
  <https://github.com/janestreet/re2>`__ for an example of use

- ``(modules_without_implementation <modules>)`` specifies a list of
  modules that have only a ``.mli`` or ``.rei`` but no ``.ml`` or
  ``.re`` file. Such modules are usually referred as *mli only
  modules*. They are not officially supported by the OCaml compiler,
  however they are commonly used. Such modules must only define
  types. Since it is not reasonably possible for dune to check
  that this is the case, dune requires the user to explicitly list
  such modules to avoid surprises. ``<modules>`` must be a subset of
  the modules listed in the ``(modules ...)`` field.

- ``(private_modules <modules>)`` specifies a list of modules that will be
  marked as private. Private modules are inaccessible from outside the libraries
  they are defined in.

- ``(allow_overlapping_dependencies)`` allows external dependencies to
  overlap with libraries that are present in the workspace

- ``(no_keep_locs)`` does nothing. It used to be a necessary hack when
  we were waiting for proper support for virtual libraries. Do not use
  in new code, it will be deleted in dune 2.0

- ``(enabled_if <blang expression>)`` allows to conditionally disable
  a library. A disabled library cannot be built and will not be
  installed. The condition is specified using the :ref:`blang`, and the
  field allows for the ``%{os_type}`` variable, which is expanded to
  the type of OS being targeted by the current build. Its value is
  the same as the value of the ``os_type`` parameter in the output of
  ``ocamlc -config``

Note that when binding C libraries, dune doesn't provide special support for
tools such as ``pkg-config``, however it integrates easily with configurator_ by
using ``(c_flags (:include ...))`` and ``(c_library_flags (:include ...))``.

.. _configurator: https://github.com/janestreet/configurator

.. _jsoo-field:

js_of_ocaml
~~~~~~~~~~~

In ``library`` and ``executables`` stanzas, you can specify ``js_of_ocaml``
options using ``(js_of_ocaml (<js_of_ocaml-options>))``.

``<js_of_ocaml-options>`` are all optional:

- ``(flags <flags>)`` to specify flags passed to ``js_of_ocaml``. This field
  supports ``(:include ...)`` forms

- ``(javascript_files (<files-list>))`` to specify ``js_of_ocaml`` JavaScript
  runtime files.

``<flags>`` is specified in the :ref:`ordered-set-language`.

The default value for ``(flags ...)`` depends on the selected build profile. The
build profile ``dev`` (the default) will enable sourcemap and the pretty
JavaScript output.

See :ref:`jsoo` for more information.

executable
----------

The ``executable`` stanza must be used to describe an executable. The
format of executable stanzas is as follows:

.. code:: scheme

    (executable
     (name <name>)
     <optional-fields>)

``<name>`` is a module name that contains the main entry point of the
executable. There can be additional modules in the current directory, you only
need to specify the entry point. Given an ``executable`` stanza with ``(name
<name>)``, dune will know how to build ``<name>.exe``, ``<name>.bc`` and
``<name>.bc.js``. ``<name>.exe`` is a native code executable, ``<name>.bc`` is a
bytecode executable which requires ``ocamlrun`` to run and ``<name>.bc.js`` is a
JavaScript generated using js_of_ocaml.

Note that in case native compilation is not available, ``<name>.exe``
will in fact be a custom byte-code executable. Custom in the sense of
``ocamlc -custom``, meaning that it is a native executable that embeds
the ``ocamlrun`` virtual machine as well as the byte code. As such you
can always rely on ``<name>.exe`` being available. Moreover, it is
usually preferable to use ``<name>.exe`` in custom rules or when
calling the executable by hand. This is because running a byte-code
executable often requires loading shared libraries that are locally
built, and so requires additional setup such as setting specific
environment variables and dune doesn't do at the moment.

Native compilation is considered not available when there is no ``ocamlopt``
binary at the same place as where ``ocamlc`` was found.

Executables can also be linked as object or shared object files. See
`linking modes`_ for more information.

``<optional-fields>`` are:

- ``(public_name <public-name>)`` specifies that the executable should be
  installed under that name. It is the same as adding the following stanza to
  your ``dune`` file:

   .. code:: scheme

       (install
        (section bin)
        (files (<name>.exe as <public-name>)))

.. _shared-exe-fields:

- ``(package <package>)`` if there is a ``(public_name ...)`` field, this
  specifies the package the executables are part of

- ``(libraries <library-dependencies>)`` specifies the library dependencies.
  See the section about :ref:`library-deps` for more details

- ``(link_flags <flags>)`` specifies additional flags to pass to the linker.
  This field supports ``(:include ...)`` forms

- ``(link_deps (<deps-conf list>))`` specifies the dependencies used only by the
  linker, for example when using a version script. See the :ref:`deps-field`
  section for more details.

- ``(modules <modules>)`` specifies which modules in the current directory
  dune should consider when building this executable. Modules not listed
  here will be ignored and cannot be used inside the executable described by
  the current stanza. It is interpreted in the same way as the ``(modules
  ...)`` field of `library`_

- ``(modes (<modes>))`` sets the `linking modes`_. The default is
  ``(byte exe)``

- ``(preprocess <preprocess-spec>)`` is the same as the ``(preprocess ...)``
  field of `library`_

- ``(preprocessor_deps (<deps-conf list>))`` is the same as the
  ``(preprocessor_deps ...)`` field of `library`_

- ``js_of_ocaml``. See the section about :ref:`jsoo-field`

- ``flags``, ``ocamlc_flags`` and ``ocamlopt_flags``. See the section about
  specifying :ref:`ocaml-flags`

- ``(modules_without_implementation <modules>)`` is the same as the
  corresponding field of `library`_

- ``(allow_overlapping_dependencies)`` is the same as the
  corresponding field of `library`_

- ``(optional)`` is the same as the corresponding field of `library`_

- ``(promote <options>)`` allows to promote the linked executables to
  the source tree. The options are the same as for the :ref:`rule
  promote mode <promote>`. Adding ``(promote (until-clean))`` to an
  ``executable`` stanza will cause Dune to copy the ``.exe`` files to
  the source tree and ``dune clean`` to delete them

Linking modes
~~~~~~~~~~~~~

The ``modes`` field allows to select what linking modes should be used
to link executables. Each mode is a pair ``(<compilation-mode>
<binary-kind>)`` where ``<compilation-mode>`` describes whether the
byte code or native code backend of the OCaml compiler should be used
and ``<binary-kind>`` describes what kind of file should be produced.

``<compilation-mode>`` must be ``byte``, ``native`` or ``best``, where
``best`` is ``native`` with a fallback to byte-code when native
compilation is not available.

``<binary-kind>`` is one of:

- ``c`` for producing OCaml bytecode embedded in a C file
- ``exe`` for normal executables
- ``object`` for producing static object files that can be manually
  linked into C applications
- ``shared_object`` for producing object files that can be dynamically
  loaded into an application. This mode can be used to write a plugin
  in OCaml for a non-OCaml application.
- ``js`` for producing Javascript from bytecode executables, see
  :ref:`explicit-js-mode`.

For instance the following ``executables`` stanza will produce byte
code executables and native shared objects:

.. code:: scheme

          (executables
           ((names (a b c))
            (modes ((byte exe) (native shared_object)))))

Additionally, you can use the following short-hands:

- ``c`` for ``(byte c)``
- ``exe`` for ``(best exe)``
- ``object`` for ``(best object)``
- ``shared_object`` for ``(best shared_object)``
- ``byte`` for ``(byte exe)``
- ``native`` for ``(native exe)``
- ``js`` for ``(byte js)``

For instance the following ``modes`` fields are all equivalent:

.. code:: scheme

          (modes (exe object shared_object))
          (modes ((best exe)
                  (best object)
                  (best shared_object)))

The extensions for the various linking modes are chosen as follows:

================ ============= =================
compilation mode binary kind   extensions
---------------- ------------- -----------------
byte             exe           .bc and .bc.js
native/best      exe           .exe
byte             object        .bc%{ext_obj}
native/best      object        .exe%{ext_obj}
byte             shared_object .bc%{ext_dll}
native/best      shared_object %{ext_dll}
byte             c             .bc.c
byte             js            .bc.js
================ ============= =================

Where ``%{ext_obj}`` and ``%{ext_dll}`` are the extensions for object
and shared object files. Their value depends on the OS, for instance
on Unix ``%{ext_obj}`` is usually ``.o`` and ``%{ext_dll}`` is usually
``.so`` while on Windows ``%{ext_obj}`` is ``.obj`` and ``%{ext_dll}``
is ``.dll``.

Note that when ``(byte exe)`` is specified but neither ``(best exe)``
nor ``(native exe)`` are specified, Dune still knows how to build
an executable with the extension ``.exe``. In such case, the ``.exe``
version is the same as the ``.bc`` one except that it is linked with
the ``-custom`` option of the compiler. You should always use the
``.exe`` rather that the ``.bc`` inside build rules.

executables
-----------

The ``executables`` stanza is the same as the ``executable`` stanza, except that
it is used to describe several executables sharing the same configuration.

It shares the same fields as the ``executable`` stanza, except that instead of
``(name ...)`` and ``(public_name ...)`` you must use:

- ``(names <names>)`` where ``<names>`` is a list of entry point names. As for
  ``executable`` you only need to specify the modules containing the entry point
  of each executable

- ``(public_names <names>)`` describes under what name each executable should
  be installed. The list of names must be of the same length as the list in the
  ``(names ...)`` field. Moreover you can use ``-`` for executables that
  shouldn't be installed

rule
----

The ``rule`` stanza is used to create custom user rules. It tells dune how
to generate a specific set of files from a specific set of dependencies.

The syntax is as follows:

.. code:: scheme

    (rule
     (target[s] <filenames>)
     (action  <action>)
     <optional-fields>)

``<filenames>`` is a list of file names (if defined with ``targets``)
or exactly one file name (if defined with ``target``). Note that
currently dune only supports user rules with targets in the current
directory.

``<action>`` is the action to run to produce the targets from the dependencies.
See the :ref:`user-actions` section for more details.

``<optional-fields>`` are:

- ``(deps <deps-conf list>)`` to specify the dependencies of the
  rule. See the :ref:`deps-field` section for more details.

- ``(mode <mode>)`` to specify how to handle the targets, see `modes`_
  for details

- ``(fallback)`` is deprecated and is the same as ``(mode fallback)``

- ``(locks (<lock-names>))`` specify that the action must be run while
  holding the following locks. See the :ref:`locks` section for more details.

Note that contrary to makefiles or other build systems, user rules currently
don't support patterns, such as a rule to produce ``%.y`` from ``%.x`` for any
given ``%``. This might be supported in the future.

modes
~~~~~

By default, the target of a rule must not exist in the source tree and
dune will error out when this is the case.

However, it is possible to change this behavior using the ``mode``
field. The following modes are available:

- ``standard``, this is the standard mode

- ``fallback``, in this mode, when the targets are already present in
  the source tree, dune will ignore the rule. It is an error if
  only a subset of the targets are present in the tree. The common use
  of fallback rules is to generate default configuration files that
  may be generated by a configure script.

.. _promote:

- ``promote`` or ``(promote <options>)``, in this mode, the files
  in the source tree will be ignored. Once the rule has been executed,
  the targets will be copied back to the source tree
  The following options are available:

  - ``(until-clean)`` means that ``dune clean`` will remove the promoted files
    from the source tree.
  - ``(into <dir>)`` means that the files are promoted in ``<dir>`` instead of
    the current directory. This feature is available since Dune 1.8.
  - ``(only <predicate>)`` means that only a subset of the targets
    should be promoted. The argument is a predicate in a syntax
    similar to the argument of :ref:`(dirs ...) <dune-subdirs>`. This
    feature is available since dune 1.10.

- ``promote-until-clean`` is the same as ``(promote (until-clean))``
- ``(promote-into <dir>)`` is the same as ``(promote (into <dir>))``
- ``(promote-until-clean-into <dir>)`` is the same as ``(promote
  (until-clean) (into <dir>))``

The ``(promote <options>)`` form is only available since Dune
1.10. Before Dune 1.10, you need to use one of the ``promote-...``
forms. The ``promote-...`` forms should disappear in Dune 2.0, so
using the more generic ``(promote <options>)`` form should be prefered
in new projects.

There are two use cases for promote rules. The first one is when the
generated code is easier to review than the generator, so it's easier
to commit the generated code and review it. The second is to cut down
dependencies during releases: by passing ``--ignore-promoted-rules``
to dune, rules will ``(mode promote)`` will be ignored and the source
files will be used instead. The ``-p/--for-release-of-packages`` flag
implies ``--ignore-promote-rules``. However, rules that promotes only
a subset of their targets via ``(only ...)`` are never ignored.

inferred rules
~~~~~~~~~~~~~~

When using the action DSL (see :ref:`user-actions`), it is most of the
time obvious what are the dependencies and targets.

For instance:

.. code:: scheme

    (rule
     (target b)
     (deps   a)
     (action (copy %{deps} %{target})))

In this example it is obvious by inspecting the action what the
dependencies and targets are. When this is the case you can use the
following shorter syntax, where dune infers dependencies and
targets for you:

.. code:: scheme

    (rule <action>)

For instance:

.. code:: scheme

    (rule (copy a b))

Note that in dune, targets must always be known
statically. For instance, this ``(rule ...)``
stanza is rejected by dune:

.. code:: scheme

    (rule (copy a b.%{read:file}))

ocamllex
--------

``(ocamllex <names>)`` is essentially a shorthand for:

.. code:: scheme

    (rule
     (target <name>.ml)
     (deps   <name>.mll)
     (action (chdir %{workspace_root}
              (run %{bin:ocamllex} -q -o %{target} %{deps}))))

To use a different rule mode, use the long form:

.. code:: scheme

    (ocamllex
     (modules <names>)
     (mode    <mode>))

.. _ocamlyacc:

ocamlyacc
---------

``(ocamlyacc <names>)`` is essentially a shorthand for:

.. code:: scheme

    (rule
     (targets <name>.ml <name>.mli)
     (deps    <name>.mly)
     (action  (chdir %{workspace_root}
               (run %{bin:ocamlyacc} %{deps}))))

To use a different rule mode, use the long form:

.. code:: scheme

    (ocamlyacc
     (modules <names>)
     (mode    <mode>))

.. _menhir:

menhir
------

A ``menhir`` stanza is available to support the menhir_ parser generator.

To use menhir in a dune project, the language version should be selected in the
``dune-project`` file. For example:

.. code:: scheme

  (using menhir 2.0)

This will enable support for menhir stanzas in the current project. If the
language version is absent, dune will automatically add this line with the
latest menhir version to the project file once a menhir stanza is used anywhere.

The basic form for defining menhir-git_ parsers (analogous to :ref:`ocamlyacc`) is:

.. code:: scheme

    (menhir
     (modules <parser1> <parser2> ...)
     <optional-fields>)

``<optional-fields>`` are:

- ``(merge_into <base_name>)`` is used to define modular parsers. This
  correspond to the ``--base`` command line option of ``menhir``. With this
  option, a single parser named ``base_name`` is generated.

- ``(flags <option1> <option2> ...)`` can be used to pass extra flags can be
  passed to menhir.

- ``(infer <bool>)`` can be used to enable using menhir with type
  inference. This option is enabled by default with Menhir language 2.0.

Menhir supports writing the grammar and automaton to ``.cmly`` file. Therefore,
if this is flag is passed to menhir, dune will know to introduce a ``.cmly``
target for the module.

.. _menhir-git: https://gitlab.inria.fr/fpottier/menhir


cinaps
------

A ``cinaps`` stanza is available to support the ``cinaps`` tool.  See
the `cinaps website <https://github.com/janestreet/cinaps>`_ for more
details.

.. _alias-stanza:

alias
-----

The ``alias`` stanza lets you add dependencies to an alias, or specify an action
to run to construct the alias.

The syntax is as follows:

.. code:: scheme

    (alias
     (name    <alias-name>)
     (deps    <deps-conf list>)
     <optional-fields>)

``<name>`` is an alias name such as ``runtest``.

.. _alias-fields:

``<deps-conf list>`` specifies the dependencies of the alias. See the
:ref:`deps-field` section for more details.

``<optional-fields>`` are:

- ``<action>``, an action to run when constructing the alias. See the
  :ref:`user-actions` section for more details.

- ``(package <name>)`` indicates that this alias stanza is part of package
  ``<name>`` and should be filtered out if ``<name>`` is filtered out from the
  command line, either with ``--only-packages <pkgs>`` or ``-p <pkgs>``

- ``(locks (<lock-names>))`` specify that the action must be run while
  holding the following locks. See the :ref:`locks` section for more details.

- ``(enabled_if <blang expression>)`` specifies the boolean condition that must
  be true for the tests to run. The condition is specified using the :ref:`blang`, and
  the field allows for :ref:`variables` to appear in the expressions.

The typical use of the ``alias`` stanza is to define tests:

.. code:: scheme

    (alias
     (name   runtest)
     (action (run %{exe:my-test-program.exe} blah)))

See the section about :ref:`running-tests` for details.

Note that if your project contains several packages and you run the tests
from the opam file using a ``build-test`` field, then all your ``runtest`` alias
stanzas should have a ``(package ...)`` field in order to partition the set of
tests.

.. _install:

install
-------

Dune supports installing packages on the system, i.e. copying freshly
built artifacts from the workspace to the system.  See the
`installation` section for more details.

Handling of the .exe extension on Windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under Microsoft Windows, executables must be suffixed with
``.exe``. Dune tries to make sure that executables are always
installed with this extension on Windows.

More precisely, when installing a file via an ``(install ...)``
stanza, if the source file has extension ``.exe`` or ``.bc``, then
dune implicitly adds the ``.exe`` extension to the destination, if
not already present.

copy_files
----------

The ``copy_files`` and ``copy_files#`` stanzas allow to specify that
files from another directory could be copied if needed to the current
directory.

The syntax is as follows:

.. code:: scheme

    (copy_files <glob>)

``<glob>`` represents the set of files to copy, see the :ref:`glob
<glob>` for details.

The difference between ``copy_files`` and ``copy_files#`` is the same
as the difference between the ``copy`` and ``copy#`` action. See the
:ref:`user-actions` section for more details.

include
-------

The ``include`` stanza allows to include the contents of another file into the
current dune file. Currently, the included file cannot be generated and must be
present in the source tree. This feature is intended to be used in conjunction
with promotion, when parts of a dune file are to be generated.

For instance:

.. code:: scheme

    (include dune.inc)

    (rule (with-stdout-to dune.inc.gen (run ./gen-dune.exe)))

    (alias
     (name   runtest)
     (action (diff dune.inc dune.inc.gen)))

With this dune file, running dune as follow will replace the
``dune.inc`` file in the source tree by the generated one:

.. code:: shell

    $ dune build @runtest --auto-promote

.. _tests-stanza:

tests
-----

The ``tests`` stanza allows one to easily define multiple tests. For example we
can define two tests at once with:

.. code:: scheme

   (tests
    (names mytest expect_test)
    <optional fields>)

This will define an executable named ``mytest.exe`` that will be executed as
part of the ``runtest`` alias. If the directory also contains an
``expect_test.expected`` file, then ``expect_test`` will be used to define an
expect test. That is, the test will be executed and its output will be compared
to ``expect_test.expected``.

The optional fields that are supported are a subset of the alias and executables
fields. In particular, all fields except for ``public_names`` are supported from
the :ref:`executables stanza <shared-exe-fields>`. Alias fields apart from
``name`` are allowed.

By default the test binaries are run without options.  The ``action`` field can
be used to override the test binary invocation, for example if you're using
alcotest and wish to see all the test failures on the standard output when
running dune runtest you can use the following stanza:

.. code:: scheme

   (tests
    (names mytest)
    (libraries alcotest mylib)
    (action (run %{test} -e)))

test
----

The ``test`` stanza is the singular form of ``tests``. The only difference is
that it's of the form:

.. code:: scheme

   (test
    (name foo)
    <optional fields>)

where the ``name`` field is singular. The same optional fields are supported.

.. _dune-env:

env
---

The ``env`` stanza allows to modify the environment. The syntax is as
follow:

.. code:: scheme

     (env
      (<profile1> <settings1>)
      (<profile2> <settings2>)
      ...
      (<profilen> <settingsn>))

The first form ``(<profile> <settings>)`` that correspond to the
selected build profile will be used to modify the environment in this
directory. You can use ``_`` to match any build profile.

Fields supported in ``<settings>`` are:

- any OCaml flags field, see :ref:`ocaml-flags` for more details.

- ``(c_flags <flags>)`` and ``(cxx_flags <flags>)``
  to specify compilation flags for C and C++ stubs, respectively.
  See `library`_ for more details.

- ``(env-vars (<var1> <val1>) .. (<varN> <valN>))``. This will add the
  corresponding variables to the environment in which the build commands are
  executed, and under which ``dune exec`` runs.


- ``(binaries <filepath> (<filepath> as <name>))``. This will make the binary at
  ``<filepath>`` as ``<name>``. If the ``<name>`` isn't provided, then it will
  be inferred from the basename of ``<filepath>`` by dropping the ``.exe``
  suffix if it exists.

- ``(inline_tests <state>)`` where state is either ``enabled``, ``disabled`` or
  ``ignored``. This field is available since Dune 1.11. It controls the value
  of the variable ``%{inline_tests}`` that is read by the inline test framework.
  The default value is ``disabled`` for the ``release`` profile and ``enabled``
  otherwise.

.. _dune-subdirs:

dirs (since 1.6)
-------------------

The ``dirs`` stanza allows to tell specify the sub-directories dune will
include in a build. The syntax is based on dune's predicate language and allows
the user the following operations:

- The special value ``:standard`` which refers to the default set of used
  directories. These are the directories that don't start with ``.`` or ``_``.

- Set operations. Differences are expressed with backslash: ``* \ bar``, unions
  are done by listing multiple items.

- Sets can be defined using globs.

Examples:

.. code:: scheme

   (dirs *) ;; include all directories
   (dirs :standard \ ocaml) ;; include all directories except ocaml
   (dirs :standard \ test* foo*) ;; exclude all directories that start with test or foo

A directory that is not included by this stanza will not be eagerly scanned by
Dune. Any ``dune`` or other special files in it won't be interpreted either and
will be treated as raw data. It is however possible to depend on files inside
ignored sub-directories.

.. _dune-data_only_dirs:

data_only_dirs (since 1.6)
--------------------------

Dune allows the user to treat directories as *data only*. Dune files in these
directories will not be evaluated for their rules, but the contents of these
directories will still be usable as dependencies for other rules.

The syntax is the same as for the ``dirs`` stanza except that ``:standard``
is by default empty.

Example:

.. code:: scheme

   ;; dune files in fixtures_* dirs are ignored
   (data_only_dirs fixtures_*)

.. _dune-ignored_subdirs:

ignored_subdirs (deprecated in 1.6)
-----------------------------------

One may also specify *data only* directories using the ``ignored_subdirs``
stanza. The meaning is the same as ``data_only_dirs`` but the syntax isn't as
flexible and only accepts a list of directory names. It is advised to switch to
the new ``data_only_dirs`` stanza.

Example:

.. code:: scheme

     (ignored_subdirs (<sub-dir1> <sub-dir2> ...))

All of the specified ``<sub-dirn>`` will be ignored by dune. Note that users
should rely on the ``dirs`` stanza along with the appropriate set operations
instead of this stanza. For example:

.. code:: scheme

  (dirs :standard \ <sub-dir1> <sub-dir2> ...)

.. _dune-vendored_dirs:

vendored_dirs (since 1.11)
--------------------------

Dune supports vendoring of other dune-based projects natively since simply
copying a project into a subdirectory of your own project will work. Simply
doing that has a few limitations though. You can workaround those by explicitly
marking such directories as containing vendored code.

Example:

.. code:: scheme

   (vendored_dirs vendor)


Dune will not resolve aliases in vendored directories meaning by default it will
not build all installable targets, run the test, format or lint the code located
in such a directory while still building the parts your project depend upon.
Libraries and executable in vendored directories will also be built with a ``-w
-a`` flag to suppress all warnings and prevent pollution of your build output.


.. _include_subdirs:

include_subdirs
---------------

The ``include_subdirs`` stanza is used to control how dune considers
sub-directories of the current directory. The syntax is as follow:

.. code:: scheme

     (include_subdirs <mode>)

Where ``<mode>`` maybe be one of:

- ``no``, the default
- ``unqualified``

When the ``include_subdirs`` stanza is not present or ``<mode>`` is
``no``, dune considers sub-directories as independent. When ``<mode>``
is ``unqualified``, dune will assume that the sub-directories of the
current directory are part of the same group of directories. In
particular, dune will scan all these directories at once when looking
for OCaml/Reason files. This allows you to split a library between
several directories. ``unqualified`` means that modules in
sub-directories are seen as if they were all in the same directory. In
particular, you cannot have two modules with the same name in two
different directories. It is planned to add a ``qualified`` mode in
the future.

Note that sub-directories are included recursively, however the
recursion will stop when encountering a sub-directory that contains
another ``include_subdirs`` stanza. Additionally, it is not allowed
for a sub-directory of a directory with ``(include_subdirs <x>)``
where ``<x>`` is not ``no`` to contain one of the following stanzas:

- ``library``
- ``executable(s)``
- ``test(s)``

toplevel
--------

The ``toplevel`` stanza allows one to define custom toplevels. Custom toplevels
automatically load a set of specified libraries and are runnable like normal
executables. Example:

.. code:: scheme

   (toplevel
    (name tt)
    (libraries str))

This will create a toplevel with the ``str`` library loaded. We may build and
run this toplevel with:

.. code:: shell

   $ dune exec ./tt.exe

external_variant
-----------------

The ``external_variant`` allow to declare a tagged implementation that does not
live inside the virtual library project.

.. code:: scheme

   (external_variant
    (variant foo)
    (implementation lib-foo)
    (virtual_library vlib))

This will add `lib-foo` to the list of known implementations of `vlib`. For more
details see :ref:`dune-variants`

.. _coq-theory:

coq.theory
----------

Dune is also able to build Coq developments. A Coq project is a mix of
Coq ``.v`` files and (optionally) OCaml libraries linking to the Coq
API (in which case we say the project is a *Coq plugin*). To enable
Coq support in a dune project, the language version should be selected
in the ``dune-project`` file. For example:

.. code:: scheme

    (using coq 0.1)

This will enable support for the ``coq.theory`` stanza in the current project. If the
language version is absent, dune will automatically add this line with the
latest Coq version to the project file once a ``(coq.theory ...)`` stanza is used anywhere.

The basic form for defining Coq libraries is very similar to the OCaml form:

.. code:: scheme

    (coq.theory
     (name <module_prefix>)
     (public_name <package.lib_name>)
     (synopsis <text>)
     (modules <ordered_set_lang>)
     (libraries <ocaml_libraries>)
     (flags <coq_flags>))

The stanza will build all `.v` files on the given directory. The semantics of fields is:

- ``<module_prefix>`` will be used as the default Coq library prefix ``-R``,
- the ``modules`` field does allow to constraint the set of modules
  included in the library, similarly to its OCaml counterpart,
- ``public_name`` will make Dune generate install rules for the `.vo`
  files; files will be installed in
  ``lib/coq/user-contrib/<module_prefix>``, as customary in the
  make-based Coq package eco-system. For compatibility, we also installs the `.cmxs`
  files appearing in `<ocaml-librarie>` under the `user-contrib` prefix.
- ``<coq_flags>`` will be passed to ``coqc``,
- the path to installed locations of ``<ocaml_libraries>`` will be passed to
  ``coqdep`` and ``coqc`` using Coq's ``-I`` flag; this allows for a Coq
  library to depend on a ML plugin.

Recursive qualification of modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you add:

.. code:: scheme

    (include_subdirs qualified)

to a ``dune`` file, Dune will to consider that all the modules in their
directory and sub-directories, adding a prefix to the module name in the usual
Coq style for sub-directories. For example, file ``A/b/C.v`` will be module
``A.b.C``.

Limitations
~~~~~~~~~~~

- composition and scoping of Coq libraries is still not possible. For now,
  libraries are located using Coq's built-in library management,
- .v always depend on the native version of a plugin,
- a ``foo.mlpack`` file must the present for locally defined plugins to work,
  this is a limitation of coqdep.

coq.pp
------

Coq plugin writers usually need to write ``.mlg`` files to extend Coq
grammar. Such files are pre-processed with `coqpp`; to help plugin
writers avoid boilerplate we provide a `(coqpp ...)` stanza:

.. code:: scheme

    (coq.pp (modules <mlg_list>))

which for each ``g_mod`` in ``<mlg_list>`` is equivalent to:

.. code:: scheme

    (rule
     (targets g_mod.ml)
     (deps (:mlg-file g_mod.mlg))
     (action (run coqpp %{mlg-file})))

.. _dune-workspace:

dune-workspace
==============

By default, a workspace has only one build context named ``default`` which
correspond to the environment in which ``dune`` is run. You can define more
contexts by writing a ``dune-workspace`` file.

You can point ``dune`` to an explicit ``dune-workspace`` file with the
``--workspace`` option. For instance it is good practice to write a
``dune-workspace.dev`` in your project with all the version of OCaml your
projects support. This way developers can tests that the code builds with all
version of OCaml by simply running:

.. code:: bash

    $ dune build --workspace dune-workspace.dev @all @runtest

The ``dune-workspace`` file uses the S-expression syntax. This is what
a typical ``dune-workspace`` file looks like:

.. code:: scheme

    (lang dune 1.0)
    (context (opam (switch 4.02.3)))
    (context (opam (switch 4.03.0)))
    (context (opam (switch 4.04.0)))

The rest of this section describe the stanzas available.

Note that an empty ``dune-workspace`` file is interpreted the same as one
containing exactly:

.. code:: scheme

    (lang dune 1.0)
    (context default)

This allows you to use an empty ``dune-workspace`` file to mark the root of your
project.

profile
-------

The build profile can be selected in the ``dune-workspace`` file by write a
``(profile ...)`` stanza. For instance:

.. code:: scheme

    (profile release)

Note that the command line option ``--profile`` has precedence over this stanza.

env
---

The ``env`` stanza can be used to set the base environment for all contexts in
this workspace. This environment has the lowest precedence of all other ``env``
stanzas. The syntax for this stanza is the same dune's :ref:`dune-env` stanza.

context
-------

The ``(context ...)`` stanza declares a build context. The argument
can be either ``default`` or ``(default)`` for the default build
context or can be the description of an opam switch, as follows:

.. code:: scheme

    (context (opam (switch <opam-switch-name>)
                   <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the name of the subdirectory of ``_build``
   where the artifacts for this build context will be stored

-  ``(root <opam-root>)`` is the opam root. By default it will take
   the opam root defined by the environment in which ``dune`` is
   run which is usually ``~/.opam``

- ``(merlin)`` instructs dune to use this build context for
  merlin

- ``(profile <profile>)`` to set a different profile for a build
  context. This has precedence over the command line option
  ``--profile``

- ``(env <env>)`` to set the environment for a particular context. This is of
  higher precedence than the root ``env`` stanza in the workspace file. This
  field the same options as the :ref:`dune-env` stanza.

- ``(toolchain <findlib_coolchain>)`` set findlib toolchain for the context.

- ``(host <host_context>)`` choose a different context to build binaries that
  are meant to be executed on the host machine, such as preprocessors.

- ``(paths (<var1> <val1>) .. (<varN> <valN>))`` allows to set the value of any
  ``PATH``-like variables in this context. If ``PATH`` itself is modified in
  this way, its value will be used to resolve binaries in the workspace,
  including finding the compiler and related tools. These variables will also be
  passed as part of the environment to any program launched by ``dune``. For
  each variable, the value is specified using the :ref:`ordered-set-language`.
  Relative paths are interpreted with respect to the workspace root, see
  :ref:`finding-root`.

Both ``(default ...)`` and ``(opam ...)`` accept a ``targets`` field in order to
setup cross compilation. See :ref:`cross-compilation` for more
information.

Merlin reads compilation artifacts and it can only read the compilation
artifacts of a single context. Usually, you should use the artifacts from the
``default`` context, and if you have the ``(context default)`` stanza in your
``dune-workspace`` file, that is the one dune will use.

For rare cases where this is not what you want, you can force dune to use a
different build contexts for merlin by adding the field ``(merlin)`` to this
context.
