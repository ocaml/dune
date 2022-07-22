****************
Stanza Reference
****************

.. _dune-project:

dune-project
============

These files are used to mark the root of projects as well as define project-wide
parameters. The first line of ``dune-project`` must be a ``lang`` stanza with no
extra whitespace or comments. The ``lang`` stanza controls the names and
contents of all configuration files read by Dune and looks like:

.. code:: scheme

   (lang dune 3.4)

Additionally, they can contains the following stanzas.

.. _using:

`using`
-------

The language of configuration files read by Dune can be extended to support
additional stanzas (eg., ``menhir``, ``coq.theory``, ``mdx``). This is done by
adding a line in the ``dune-project`` file, such as:

.. code:: scheme

    (using <plugin> <version>)

Here, ``<plugin>`` is the name of the plugin that
defines this stanza and ``<version>`` describes the configuration language's version.
Note that this version has nothing to do with the version of the
associated tool or library. In particular, adding a ``using`` stanza will not
result in a build dependency in the generated ``.opam`` file. See
:ref:`generate_opam_files <generate_opam_files>`.

name
----

Sets the name of the project. It's used by :ref:`dune subst <dune-subst>`
and error messages.

.. code:: scheme

    (name <name>)

version
-------

Sets the version of the project:

.. code:: scheme

    (version <version>)

cram
----

Enable or disable cram-style tests for the project. See :ref:`cram-tests` for
details.

.. code:: scheme

   (cram <status>)

Where status is either ``enabled`` or ``disabled``.

.. _implicit_transitive_deps:

implicit_transitive_deps
------------------------

By default, Dune allows transitive dependencies of dependencies used
when compiling OCaml; however, this setting can be controlled per
project:

.. code:: scheme

    (implicit_transitive_deps <bool>)

When set to ``false``, all dependencies directly used by a library
or an executable must be added in the ``libraries`` field. We
recommend users experiment with this mode and report any problems.

Note that you must use ``threads.posix`` instead of ``threads`` when using this
mode. This isn't an important limitation, as ``threads.vm`` are deprecated
anyways.

In some situations, it's desirable to selectively preserve the
behavior of transitive dependencies' availability to users of a
library. For example, if we define a library ``foo_more``, that
extends ``foo``, we might want ``foo_more`` users to immediately
have ``foo`` available as well. To do this, we must define the
dependency on ``foo`` as re-exported:

.. code:: scheme

   (library
    (name foo_more)
    (libraries (re_export foo)))

.. _wrapped-executables:

wrapped_executables
-------------------

Executables are made of compilation units whose names may collide with libraries'
compilation units. To avoid this possibility, Dune prefixes these
compilation unit names with ``Dune__exe__``. This is entirely transparent to
users except when such executables are debugged. In which case, the mangled
names will be visible in the debugger.

Starting from Dune 1.11, an option is available to turn on/off name mangling for
executables on a per-project basis:

.. code:: scheme

    (wrapped_executables <bool>)

Starting with Dune 2.0, Dune mangles compilation units of executables by
default. However, this can still be turned off using ``(wrapped_executables
false)``

.. _executables_implicit_empty_intf:

executables_implicit_empty_intf
-------------------------------

By default, executables defined via ``(executables(s) ...)`` or ``(test(s)
...)`` stanzas are compiled with the interface file provided (e.g., ``.mli`` or
``rei``). Since these modules cannot be used as library dependencies, it's
common to give them empty interface files to strengthen the compiler's ability
to detect unused values in these modules.

Starting from Dune 2.9, an option is available to automatically generate empty
interface files for executables and tests that don't already have them:

.. code:: scheme

    (executables_implicit_empty_intf true)

This option is enabled by default starting with Dune lang 3.0, so
empty interface files are no longer needed.

.. _explicit-js-mode:

explicit_js_mode
----------------

Traditionally, JavaScript targets were defined for every bytecode executable.
This wasn't very precise and didn't interact well with the ``@all`` alias.

You can opt out of this behaviour by using:

.. code:: scheme

    (explicit_js_mode)

When this mode is enabled, an explicit ``js`` mode needs to be added to the
``(modes ...)`` field of executables in order to trigger the JavaScript
compilation. Explicit JS targets declared like this will be attached to the
``@all`` alias.

Starting with Dune 2.0, this behaviour is the default, and there is no way to
disable it.

expand_aliases_in_sandbox
-------------------------

When a sandboxed action depends on a alias, copy the expansion of the
alias inside the sandbox. For instance, in the following example:

.. code:: scheme

    (alias
     (name foo)
     (deps ../x))

    (cram
     (deps (alias foo)))

File `x` will be visible inside the cram test if and only if this
option is enabled. This option is a better default in general, however
it currently causes cram tests to run noticeably slower. So it is
disabled by default until the performance issue with cram test is
fixed.

.. _dialect:

dialect
-------

A dialect is an alternative frontend to OCaml (such as ReasonML). It's
described by a pair of file extensions, one corresponding to interfaces and one
to implementations.

A dialect can use the standard OCaml syntax, or it can specify an action to
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

For interfaces and implementations, ``(extension <string>)`` specifies the file extension used for this dialect.
The extension string must not contain any dots
and be unique in a given project (so that a given extension can be mapped back
to a corresponding dialect).

``<optional fields>`` are:

- Run ``(preprocess <action>)`` to produce a valid OCaml
  abstract syntax tree. It's expected to read the file given in the variable
  named ``input-file`` and output a *binary* abstract syntax tree on its
  standard output. See :ref:`preprocessing-actions` for more information.

  If the field isn't present, it's assumed that the corresponding source code
  is already valid OCaml code and can be passed to the OCaml compiler as-is.


- Run ``(format <action>)`` to format source code for this
  dialect. The action is expected to read the file given in the variable named
  ``input-file`` and output the formatted source code on its standard
  output. For more information. See :ref:`formatting-main` for more information.

  If the field is not present, then ``(preprocess <action>)`` is also not present
  (so that the dialect consists of valid OCaml code). In that case, the
  dialect will be formatted as any other OCaml code by default. Otherwise no special
  formatting will be done.

.. _formatting:

formatting
----------

Starting in Dune 2.0, :ref:`formatting-main` is automatically enabled. This can be
controlled by using

.. code:: scheme

    (formatting <setting>)

where ``<setting>`` is one of:

- ``disabled``, meaning that automatic formatting is disabled

- ``(enabled_for <languages>)`` can be used to restrict the languages that are
  considered for formatting.

.. _subst:

subst
-----

Starting in Dune 3.0, :ref:`dune-subst` can be explicitly disabled or enabled.
By default it is enabled and controlled by using:

.. code:: scheme

    (subst <setting>)

where ``<setting>`` is one of:

- ``disabled``, meaning that any call of `dune subst` in this project is
  forbidden and will result in an error.

- ``enabled``, allowing substitutions explicitly. This is the default.


.. _generate_opam_files:

generate_opam_files
-------------------

Dune is able to use metadata specified in the ``dune-project`` file to generate
``.opam`` files (see :ref:`opam-generation`). To enable this integration, add the
following field to the ``dune-project`` file:

.. code:: scheme

   (generate_opam_files true)

Dune uses the following global fields to set the metadata for all packages
defined in the project:

- ``(license <names>)`` - specifies the license of the project, ideally as an
  identifier from the `SPDX License List <https://spdx.org/licenses/>`__.
  Multiple licenses may be specified.

- ``(authors <author> ..)`` - authors as inline strings

- ``(maintainers <maintainer> ..)`` - maintainers as inline strings

- ``(source <source>)`` - specifies where the source for the package can be
  found. It can be specified as ``(uri <uri>)`` or using shortcuts for some
  hosting services:

.. list-table::

  * - Service
    - Syntax
  * - `Github <https://github.com>`_
    - ``(github user/repo)``
  * - `Bitbucket <https://bitbucket.org>`_
    - ``(bitbucket user/repo)``
  * - `Gitlab <https://gitlab.com>`_
    - ``(gitlab user/repo)``
  * - `Sourcehut <https://sr.ht>`_
    - ``(sourcehut user/repo)``

- ``(bug_reports <url>)`` - where to report bugs. If a hosting service is used
  in ``(source)``, a default value is provided.

- ``(homepage <url>)`` - the homepage of the project. If a hosting service is
  used in ``(source)``, a default value is provided.

- ``(documentation <url>)`` - where the documentation is hosted

With these fields, every time one calls Dune to execute some rules (either via
``dune build``, ``dune runtest``, or something else), the opam files get
generated.

Some or all of these fields may be overridden for each package of the project, see
:ref:`package`.

.. _package:

package
-------

Package specific information is specified in the ``(package <package-fields>)`` stanza.
It contains the following fields:

- ``(name <string>)`` is the name of the package. This must be specified.

- ``(synopsis <string>)`` is a short package description.

- ``(description <string>)`` is a longer package description.

- ``(depends <dep-specification>)`` are package dependencies.

- ``(conflicts <dep-specification)`` are package conflicts.

- ``(depopts <dep-specification)`` are optional package dependencies.

- ``(tags <tags>)`` are the list of tags for the package.

- ``(deprecated_package_names <name list>)`` is a list of names that can be used
  with the :ref:`deprecated-library-name` stanza to migrate legacy libraries
  from other build systems that don't follow Dune's convention of prefixing
  the library's public name with the package name.

- ``(license <name>)``, ``(authors <authors>)``, ``(maintainers
  <maintainers>)``, ``(source <source>)``, ``(bug_reports <url>)``, ``(homepage
  <url>)``, and ``(documentation <url>)`` are the same (and take precedence over)
  the corresponding global fields. These fields have been available since Dune 2.0.

- ``(sites (<section> <name>) ...)`` define a site named ``<name>`` in the
  section ``<section>``.

Adding libraries to different packages is done via the ``public_name`` field. See
:ref:`library` section for details.

The list of dependencies ``<dep-specification>`` is modeled after opam's own
language. The syntax is a list of the following elements:

.. code::

   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   filter := :dev | :build | :with-test | :with-doc | :post

   constr := (<op> <version>)

   logop := or | and

   dep := name
        | (name <filter>)
        | (name <constr>)
        | (name (<logop> (<filter> | <constr>))*)

   dep-specification = dep+

Filters will expand to any opam variable name if prefixed by ``:``, not just the
ones listed above. This also applies to version numbers. For example, to
generate ``depends: [ pkg { = version } ]``, use ``(depends (pkg (=
:version)))``.

Note that the use of a ``using`` stanza (see :ref:`using <using>`) doesn't
automatically add the associated library or tool as a dependency. They have to
be added explicitly.

.. _always-add-cflags:

use_standard_c_and_cxx_flags
----------------------------

Since Dune 2.8, it's possible to deactivate the systematic prepending of flags
coming from ``ocamlc -config`` to the C compiler command line. This is done
adding the following field to the ``dune-project`` file:

.. code:: scheme

    (use_standard_c_and_cxx_flags true)

In this mode, Dune will populate the ``:standard`` set of C flags with the
content of ``ocamlc_cflags`` and  ``ocamlc_cppflags``. These flags can be
completed or overridden using the :ref:`ordered-set-language`. The value
``true`` is the default for Dune 3.0.

accept_alternative_dune_file_name
---------------------------------

Since Dune 3.0, it's possible to use the alternative filename ``dune-file``
instead of ``dune`` to specify the build. This may be useful to avoid problems
with ``dune`` files that have the executable permission in a directory
in the ``PATH``, which can unwittingly happen in Windows.

The feature must be enabled explicitly by adding the following field to
``dune-project``:

.. code:: scheme

   (accept_alternative_dune_file_name)

Note that ``dune`` continues to be accepted even after enabling this option, but
if a file named ``dune-file`` is found in a directory, it will take precedence
over ``dune``.

.. _dune-files:

dune
====

``dune`` files are the main part of Dune. They are used to describe libraries,
executables, tests, and everything Dune needs to know about.

The syntax of ``dune`` files is described in :ref:`metadata-format` section.

``dune`` files are composed of stanzas, as shown below:

.. code:: lisp

    (library
     (name mylib)
     (libraries base lwt))

    (rule
     (target foo.ml)
     (deps   generator/gen.exe)
     (action (run %{deps} -o %{target})))

The following sections describe the available stanzas and their meanings.

jbuild_version
--------------

Deprecated. This `jbuild_version` stanza is no longer used and will be removed
in the future.

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
module name, but it doesn't need to start with an uppercase letter.

For instance, the modules of a library named ``foo`` will be
available as ``Foo.XXX``, outside of ``foo`` itself; however, it is
allowed to write an explicit ``Foo`` module, which will
be the library interface. You are free to expose only the
modules you want.

Please note: by default, libraries and other things that consume
OCaml/Reason modules only consume modules from the directory where the
stanza appear. In order to declare a multi-directory library, you need
to use the :ref:`include_subdirs` stanza.

``<optional-fields>`` are:

- ``(public_name <name>)`` - the name under which the library can be
  referred as a dependency when it's not part of the current workspace,
  i.e., when it's installed. Without a ``(public_name ...)`` field, the library
  won't be installed by Dune. The public name must start with the package
  name it's part of and optionally followed by a dot, then anything else you
  want. The package name must also be one of the packages that Dune knows about,
  as determined by the :ref:`opam-files`

- ``(package <package>)`` installs a private library under the specified package.
  Such a library is now usable by public libraries defined in the same project.
  The Findlib name for this library will be ``<package>.__private__.<name>``;
  however, the library's interface will be hidden from consumers outside the
  project.

- ``(synopsis <string>)`` should give a one-line description of the library.
  This is used by tools that list installed libraries

- ``(modules <modules>)`` specifies what modules are part of the library. By
  default, Dune will use all the ``.ml/.re`` files in the same directory as the
  ``dune`` file. This includes ones present in the file system as well
  as ones generated by user rules. You can restrict this list by using a
  ``(modules <modules>)`` field. ``<modules>`` uses the :ref:`ordered-set-language`,
  where elements are module names and don't need to start with an uppercase
  letter. For instance, to exclude module ``Foo``, use ``(modules (:standard \
  foo))``

- ``(libraries <library-dependencies>)`` specifies the library's dependencies.
  See the section about :ref:`library-deps` for more details.

- ``(wrapped <boolean>)`` specifies whether the library modules should be
  available only through the top-level library module, or if they should all be exposed
  at the top level. The default is ``true``, and it's highly recommended to keep
  it this way. Because OCaml top-level modules must all be unique when linking
  an executables, polluting the top-level namespace will make your library
  unusable with other libraries if there is a module name clash. This option is
  only intended for libraries that manually prefix all their modules by the
  library name and to ease porting of existing projects to Dune.

- ``(wrapped (transition <message>))`` is the same as ``(wrapped true)``, except
  it will also generate unwrapped (not prefixed by the library name)
  modules to preserve compatibility. This is useful for libraries that would
  like to transition from ``(wrapped false)`` to ``(wrapped true)`` without
  breaking compatibility for users. The deprecation notices for the unwrapped
  modules will include ``<message>``.

- ``(preprocess <preprocess-spec>)`` specifies how to preprocess files when
  needed. The default is ``no_preprocessing``, and other options are described in the
  :ref:`preprocessing-spec` section.

- ``(preprocessor_deps (<deps-conf list>))`` specifies extra preprocessor dependencies
  preprocessor, i.e., if the preprocessor reads a generated file. The
  specification of dependencies is described in the :ref:`deps-field`
  section.

- ``(optional)`` - if present, it indicates that the library should only be built
  and installed if all the dependencies are available, either in the workspace
  or in the installed world. Use this to provide extra features without
  adding hard dependencies to your project

- ``(foreign_stubs <foreign-stubs-spec>)`` specifies foreign source files, e.g.,
  C or C++ stubs, to be compiled and packaged together with the library. See
  the section :ref:`foreign-sources-and-archives` for more details. This field
  replaces the now-deleted fields ``c_names``, ``c_flags``, ``cxx_names``,
  and ``cxx_flags``.

- ``(foreign_archives <foreign-archives-list>)`` specifies archives of foreign
  object files to be packaged with the library. See the section
  :ref:`foreign-archives` for more details. This field replaces the now-deleted
  field ``self_build_stubs_archive``.

- ``(install_c_headers (<names>))`` - if your library has public C header files
  that must be installed, you must list them in this field, without the ``.h``
  extension.

- ``(modes <modes>)`` is for modes which should be built by default. The
  most common use for this feature is to disable native compilation
  when writing libraries for the OCaml toplevel. The following modes
  are available: ``byte``, ``native``, and ``best``. ``best`` is
  ``native`` or ``byte`` when native compilation isn't available.

- ``(no_dynlink)`` disables dynamic linking of the library. This is for
  advanced use only. By default, you shouldn't set this option.

- ``(kind <kind>)`` sets the type of library. The default is ``normal``, but other
  available choices are ``ppx_rewriter`` and ``ppx_deriver``. They must be set
  when the library is intended to be used as a ppx rewriter or a ``[@@deriving
  ...]`` plugin. The reason ``ppx_rewriter`` and ``ppx_deriver`` are split
  is historical, and hopefully we won't need two options soon. Both ppx kinds
  support an optional field: ``(cookies <cookies>)``, where ``<cookies>`` is a
  list of pairs ``(<name> <value>)`` with ``<name>`` being the cookie name and
  ``<value>`` a string that supports :ref:`variables` evaluated
  by each preprocessor invocation (note: libraries that share
  cookies with the same name should agree on their expanded value).

- ``(ppx_runtime_libraries (<library-names>))`` is for when the library is a ``ppx
  rewriter`` or a ``[@@deriving ...]`` plugin, and has runtime dependencies. You
  need to specify these runtime dependencies here.

- ``(virtual_deps (<opam-packages>)``. Sometimes opam packages enable a specific
  feature only if another package is installed. For instance, the case of
  ``ctypes`` will only install ``ctypes.foreign`` if the dummy
  ``ctypes-foreign`` package is installed. You can specify such virtual
  dependencies here, but you don't need to do so unless you use Dune to
  synthesize the ``depends`` and ``depopts`` sections of your opam file.

- ``js_of_ocaml`` sets options for JavaScript compilation, see :ref:`jsoo-field`.

- For ``flags``, ``ocamlc_flags``, and ``ocamlopt_flags``, see the section about
  :ref:`ocaml-flags`

- ``(library_flags (<flags>))`` is a list of flags passed to
  ``ocamlc`` and ``ocamlopt`` when building the library archive files. You can
  use this to specify ``-linkall``, for instance. ``<flags>`` is a list of
  strings supporting :ref:`variables`.

- ``(c_library_flags <flags>)`` specifies the flags passed to the C compiler
  when constructing the library archive file for the C stubs. ``<flags>`` uses
  the :ref:`ordered-set-language` and supports ``(:include ...)`` forms. When you
  write bindings for a C library named ``bar``, you should typically write
  ``-lbar`` here, or whatever flags are necessary to link against this
  library.

- ``(modules_without_implementation <modules>)`` specifies a list of
  modules that have only a ``.mli`` or ``.rei`` but no ``.ml`` or
  ``.re`` file. Such modules are usually referred as *mli only
  modules*. They are not officially supported by the OCaml compiler,
  however they are commonly used. Such modules must only define
  types. Since it isn't reasonably possible for Dune to check
  this is the case, Dune requires the user to explicitly list
  such modules to avoid surprises.  Note that the
  ``modules_without_implementation`` field isn't merged in ``modules``, which
  represents the total set of modules in a library. If a directory has more
  than one stanza, and thus a ``modules`` field must be specified, ``<modules>``
  still needs to be added in ``modules``.

- ``(private_modules <modules>)`` specifies a list of modules that will be
  marked as private. Private modules are inaccessible from outside the libraries
  they are defined in. Note that the ``private_modules`` field is not merged in
  ``modules``, which represents the total set of modules in a library. If a
  directory has more than one stanza and thus a ``modules`` field must be
  specified, ``<modules>`` still need to be added in ``modules``.

- ``(allow_overlapping_dependencies)`` allows external dependencies to
  overlap with libraries that are present in the workspace

- ``(enabled_if <blang expression>)`` conditionally disables
  a library. A disabled library cannot be built and will not be
  installed. The condition is specified using the :ref:`blang`, and the
  field allows for the ``%{os_type}`` variable, which is expanded to
  the type of OS being targeted by the current build. Its value is
  the same as the value of the ``os_type`` parameter in the output of
  ``ocamlc -config``

- ``(inline_tests)`` enables inline tests for this library. They can be
  configured through options using ``(inline_tests <options>)``. See
  :ref:`inline_tests` for a reference of corresponding options.

- ``(root_module <module>)`` this field instructs dune to generate a module that
  will contain module aliases for every library specified in dependencies. This
  is useful whenever a library is shadowed by a local module. The library may
  then still be accessible via this root module

- ``(ctypes <ctypes stanza>)`` instructs dune to use ctypes stubgen to process
  your type and function descriptions for binding system libraries, vendored
  libraries, or other foreign code.  See :ref:`ctypes-stubgen` for a full
  reference. This field is available since the 3.0 version of the dune language.

- ``(empty_module_interface_if_absent)`` causes the generation of empty
  interfaces for every module that does not have an interface file already.
  Useful when modules are used solely for their side-effects. This field is
  available since the 3.0 version of the dune language.

Note that when binding C libraries, dune doesn't provide special support for
tools such as ``pkg-config``, however it integrates easily with
:ref:`configurator` by
using ``(c_flags (:include ...))`` and ``(c_library_flags (:include ...))``.

.. _foreign_library:

foreign_library
---------------

The ``foreign_library`` stanza describes archives of separately compiled
foreign object files that can be packaged with an OCaml library or linked
into an OCaml executable. See :ref:`foreign-sources-and-archives` for
further details and examples.

.. _jsoo-field:

js_of_ocaml
~~~~~~~~~~~

In ``library`` and ``executables`` stanzas, you can specify ``js_of_ocaml``
options using ``(js_of_ocaml (<js_of_ocaml-options>))``.

``<js_of_ocaml-options>`` are all optional:

- ``(flags <flags>)`` to specify flags passed to ``js_of_ocaml compile``. This field
  supports ``(:include ...)`` forms

- ``(build_runtime_flags <flags>)`` to specify flags passed to ``js_of_ocaml build-runtime``. This field
  supports ``(:include ...)`` forms

- ``(link_flags <flags>)`` to specify flags passed to ``js_of_ocaml link``. This field
  supports ``(:include ...)`` forms

- ``(javascript_files (<files-list>))`` to specify ``js_of_ocaml`` JavaScript
  runtime files.

``<flags>`` is specified in the :ref:`ordered-set-language`.

The default value for ``(flags ...)`` depends on the selected build profile. The
build profile ``dev`` (the default) will enable sourcemap and the pretty
JavaScript output.

See :ref:`jsoo` for more information.

.. _deprecated-library-name:

deprecated_library_name
-----------------------

The ``deprecated_library_name`` stanza enables redirecting an old
deprecated name after a library has been renamed. It's syntax is as
follows:

.. code:: scheme

    (deprecated_library_name
     (old_public_name <name>)
     (new_public_name <name>))

When a developer uses the old public name in a list of library
dependencies, it will be transparently replaced by the new name. Note
that it's not necessary for the new name to exist at definition time,
as it is only resolved at the point where the old name is used.

The ``old_public_name`` can also be one of the names declared in the
``deprecated_package_names`` field of the package declaration in the
``dune-project`` file. In this case, the "old" library is understood to be a
library whose name is not prefixed by the package name. Such a library cannot be
defined in Dune, but other build systems allow it. This feature is meant to
help migration from those systems.

.. _executable:

executable
----------

The ``executable`` stanza must be used to describe an executable. The
format of executable stanzas is as follows:

.. code:: scheme

    (executable
     (name <name>)
     <optional-fields>)

``<name>`` is a module name that contains the executable's main entry point.
There can be additional modules in the current directory;
you only need to specify the entry point. Given an ``executable``
stanza with ``(name <name>)``, Dune will know how to build
``<name>.exe``. If requested, it will also know how to build
``<name>.bc`` and ``<name>.bc.js`` (Dune 2.0 and up also need specific
configuration (see the ``modes`` optional field below).

``<name>.exe`` is a native code executable, ``<name>.bc`` is a bytecode executable
which requires ``ocamlrun`` to run, and ``<name>.bc.js`` is a JavaScript
generated using ``js_of_ocaml``.

Please note: in case native compilation is not available, ``<name>.exe``
will be a custom bytecode executable, in the sense of
``ocamlc -custom``. This means it's a native executable that embeds
the ``ocamlrun`` virtual machine as well as the bytecode, so you
can always rely on ``<name>.exe`` being available. Moreover, it is
usually preferable to use ``<name>.exe`` in custom rules or when
calling the executable by hand because running a bytecode
executable often requires loading shared libraries that are locally
built. This requires additional setup, such as setting specific
environment variables, which Dune doesn't do at the moment.

Native compilation isn't available when there is no ``ocamlopt``
binary at the same place as ``ocamlc`` was found.

Executables can also be linked as object or shared object files. See
`linking modes`_ for more information.

Starting from Dune 3.0, it's possible to automatically generate empty interface
files for executables. See `executables_implicit_empty_intf`_.

``<optional-fields>`` are:

- ``(public_name <public-name>)`` specifies that the executable should be
  installed under this name. It's the same as adding the following stanza to
  your ``dune`` file:

   .. code:: scheme

       (install
        (section bin)
        (files (<name>.exe as <public-name>)))

.. _shared-exe-fields:

- ``(package <package>)`` if there is a ``(public_name ...)`` field, this
  specifies the package the executables are part of it.

- ``(libraries <library-dependencies>)`` specifies the library dependencies.
  See the section about :ref:`library-deps` for more details.

- ``(link_flags <flags>)`` specifies additional flags to pass to the linker.
  This field supports ``(:include ...)`` forms.

- ``(link_deps (<deps-conf list>))`` specifies the dependencies used only by the
  linker, i.e., when using a version script. See the :ref:`deps-field`
  section for more details.

- ``(modules <modules>)`` specifies which modules in the current directory
  Dune should consider when building this executable. Modules not listed
  here will be ignored and cannot be used inside the executable described by
  the current stanza. It is interpreted in the same way as the ``(modules
  ...)`` field of `library`_.

- ``(root_module <module>)`` specifies a ``root_module`` that collects all
  listed dependencies in ``libraries``. See the documentation for
  ``root_module`` in the library stanza.

- ``(modes (<modes>))`` sets the `linking modes`_. The default is
  ``(exe)``. Before Dune 2.0, it formerly was ``(byte exe)``.

- ``(preprocess <preprocess-spec>)`` is the same as the ``(preprocess ...)``
  field of `library`_.

- ``(preprocessor_deps (<deps-conf list>))`` is the same as the
  ``(preprocessor_deps ...)`` field of `library`_.

- ``js_of_ocaml``: See the section about :ref:`jsoo-field`

- ``flags``, ``ocamlc_flags``, and ``ocamlopt_flags``: See the section about
  specifying :ref:`ocaml-flags`.

- ``(modules_without_implementation <modules>)`` is the same as the
  corresponding field of `library`_.

- ``(allow_overlapping_dependencies)`` is the same as the
  corresponding field of `library`_.

- ``(optional)`` is the same as the corresponding field of `library`_.

- ``(enabled_if <blang expression>)`` is the same as the corresponding field of `library`_.

- ``(promote <options>)`` allows promoting the linked executables to
  the source tree. The options are the same as for the :ref:`rule
  promote mode <promote>`. Adding ``(promote (until-clean))`` to an
  ``executable`` stanza will cause Dune to copy the ``.exe`` files to
  the source tree and use ``dune clean`` to delete them.

- ``(foreign_stubs <foreign-stubs-spec>)`` specifies foreign source
  files, e.g., C or C++ stubs, to be linked into the executable. See the
  section :ref:`foreign-sources-and-archives` for more details.

- ``(foreign_archives <foreign-archives-list>)`` specifies archives of
  foreign object files to be linked into the executable. See the section
  :ref:`foreign-archives` for more details.

- ``(forbidden_libraries <libraries>)`` ensures that the given
  libraries are not linked in the resulting executable. If they end up
  being pulled in, either through a direct or transitive dependency,
  Dune fails with an error message explaining how the library was
  pulled in. This field has been available since Dune 2.0.

- ``(embed_in_plugin_libraries <library-list>)`` specifies a list of libraries
  to link statically when using the ``plugin`` linking mode. By default, no
  libraries are linked in. Note that you may need to also use the ``-linkall``
  flag if some of the libraries listed here are not referenced from any of the
  plugin modules.

- ``(ctypes <ctypes stanza>)`` instructs dune to use ctypes stubgen to process
  your type and function descriptions for binding system libraries, vendored
  libraries, or other foreign code.  See :ref:`ctypes-stubgen` for a full
  reference. This field is available since the 3.0 version of the dune language.

- ``(empty_module_interface_if_absent)`` causes the generation of empty
  interfaces for every module that does not have an interface file already.
  Useful when modules are used solely for their side-effects. This field is
  available since the 3.0 version of the Dune language.

Linking Modes
~~~~~~~~~~~~~

The ``modes`` field allows selecting which linking modes will be used
to link executables. Each mode is a pair ``(<compilation-mode>
<binary-kind>)``, where ``<compilation-mode>`` describes whether the
bytecode or native code backend of the OCaml compiler should be used
and ``<binary-kind>`` describes what kind of file should be produced.

``<compilation-mode>`` must be ``byte``, ``native``, or ``best``, where
``best`` is ``native`` with a fallback to bytecode when native
compilation isn't available.

``<binary-kind>`` is one of:

- ``c`` for producing OCaml bytecode embedded in a C file
- ``exe`` for normal executables
- ``object`` for producing static object files that can be manually
  linked into C applications
- ``shared_object`` for producing object files that can be dynamically
  loaded into an application. This mode can be used to write a plugin
  in OCaml for a non-OCaml application.
- ``js`` for producing JavaScript from bytecode executables, see
  :ref:`explicit-js-mode`.
- ``plugin`` for producing a plugin (``.cmxs`` if native or ``.cma``
  if bytecode).

For instance the following ``executables`` stanza will produce bytecode
executables and native shared objects:

.. code:: scheme

          (executables
            (names a b c)
            (modes (byte exe) (native shared_object)))

Additionally, you can use the following shorthands:

- ``c`` for ``(byte c)``
- ``exe`` for ``(best exe)``
- ``object`` for ``(best object)``
- ``shared_object`` for ``(best shared_object)``
- ``byte`` for ``(byte exe)``
- ``native`` for ``(native exe)``
- ``js`` for ``(byte js)``
- ``plugin`` for ``(best plugin)``

For instance, the following ``modes`` fields are all equivalent:

.. code:: scheme

          (modes (exe object shared_object))
          (modes ((best exe)
                  (best object)
                  (best shared_object)))

Lastly, use the special mode ``byte_complete`` for
building a bytecode executable as a native self-contained
executable, i.e., an executable that doesn't require the ``ocamlrun``
program to run and doesn't require the C stubs to be installed as
shared object files.

The extensions for the various linking modes are chosen as follows:

=========================== =================
linking mode                extensions
--------------------------- -----------------
byte                        .bc
native/best                 .exe
byte_complete               .bc.exe
(byte object)               .bc%{ext_obj}
(native/best object)        .exe%{ext_obj}
(byte shared_object)        .bc%{ext_dll}
(native/best shared_object) %{ext_dll}
c                           .bc.c
js                          .bc.js
(best plugin)               %{ext_plugin}
(byte plugin)               .cma
(native plugin)             .cmxs
=========================== =================

``%{ext_obj}`` and ``%{ext_dll}`` are the extensions for object
and shared object files. Their value depends on the OS. For instance,
on Unix ``%{ext_obj}`` is usually ``.o`` and ``%{ext_dll}`` is usually
``.so``, while on Windows ``%{ext_obj}`` is ``.obj`` and ``%{ext_dll}``
is ``.dll``.

Up to version 3.0 of the Dune language, when ``byte`` is specified but
none of ``native``, ``exe``, or ``byte_complete`` are specified, Dune
implicitly adds a linking mode that's the same as ``byte_complete``,
but it uses the extension ``.exe``. ``.bc`` files require addition al
files at runtime that aren't currently tracked by Dune, so don't
run ``.bc`` files during the build. Run the ``.bc.exe`` or
``.exe`` ones instead, as these are self-contained.

Lastly, note that ``.bc`` executables cannot contain C stubs. If your
executable contains C stubs you may want to use ``(modes exe)``.

executables
-----------

There is a very subtle difference in the naming of these stanzas. One is
``executables``, plural, and the other is ``executable``, singular.
The ``executables`` stanza is the same as the ``executable`` stanza except that
it's used to describe several executables sharing the same configuration, so the
plura ``executables`` stanza is used to describe more than one executable.

It shares the same fields as the ``executable`` stanza, except that instead of
``(name ...)`` and ``(public_name ...)`` you must use the plural versions as well:

- ``(names <names>)`` where ``<names>`` is a list of entry point names. Compare with
  ``executable`` where you only need to specify the modules containing the entry point
  of each executable.

- ``(public_names <names>)`` describes under what name to install each executable.
  The list of names must be of the same length as the list in the
  ``(names ...)`` field. Moreover, you can use ``-`` for executables that
  shouldn't be installed.

rule
----

The ``rule`` stanza is used to create custom user rules. It tells Dune how
to generate a specific set of files from a specific set of dependencies.

The syntax is as follows:

.. code:: scheme

    (rule
     (action <action>)
     <optional-fields>)

``<action>`` is what you run to produce the targets from the dependencies.
See the :ref:`user-actions` section for more details.

``<optional-fields>`` are:

- ``(target <filename>)`` or ``(targets <filenames>) ``<filenames>`` is a list
  of filenames (if defined with ``targets``) or exactly one filename (if defined
  with ``target``). Dune needs to statically know targets of each rule.
  ``(targets)`` can be omitted if it can be inferred from the action. See
  `inferred rules`_.

- ``(deps <deps-conf list>)``, to specify the dependencies of the
  rule. See the :ref:`deps-field` section for more details.

- ``(mode <mode>)``, to specify how to handle the targets. See `modes`_
  for details.

- ``(fallback)`` is deprecated and is the same as ``(mode fallback)``.

- ``(locks (<lock-names>))`` specifies that the action must be run while
  holding the following locks. See the :ref:`locks` section for more details.

- ``(alias <alias-name>)`` specifies this rule's alias. Building this
  alias means building the targets of this rule.

- ``(package <package>)`` specifies this rule's package. This rule
  will be unavailable when installing other packages in release mode.

- ``(enabled_if <blang expression>)`` specifies the Boolean condition that must
  be true for the rule to be considered. The condition is specified using the :ref:`blang`, and
  the field allows for :ref:`variables` to appear in the expressions.

Please note: contrary to makefiles or other build systems, user rules currently
don't support patterns, such as a rule to produce ``%.y`` from ``%.x`` for any
given ``%``. This might be supported in the future.

modes
~~~~~

By default, a rule's target must not exist in the source tree because
Dune will error out when this is the case; however, it's possible to change
this behavior using the ``mode`` field. The following modes are available:

- ``standard`` - the standard mode.

- ``fallback`` - in this mode, when the targets are already present in
  the source tree, Dune will ignore the rule. It's an error if
  only a subset of the targets are present in the tree. Fallback rules are
  commonly used to generate default configuration files that
  may be generated by a configure script.

.. _promote:

- ``promote`` or ``(promote <options>)`` - in this mode, the files
  in the source tree will be ignored. Once the rule has been executed,
  the targets will be copied back to the source tree.
  The following options are available:

  - ``(until-clean)`` means that ``dune clean`` will remove the promoted files
    from the source tree.
  - ``(into <dir>)`` means that the files are promoted in ``<dir>`` instead of
    the current directory. This feature has been available since Dune 1.8.
  - ``(only <predicate>)`` means that only a subset of the targets should be
    promoted. The argument is similar to the argument of :ref:`(dirs ...)
    <dune-subdirs>`, specified using the :ref:`predicate-lang`. This feature
    has been available since Dune 1.10.

There are two use cases for ``promote`` rules. The first one is when the
generated code is easier to review than the generator, so it's easier
to commit the generated code and review it. The second is to cut down
dependencies during releases. By passing ``--ignore-promoted-rules``
to Dune, rules with ``(mode promote)`` will be ignored, and the source
files will be used instead. The ``-p/--for-release-of-packages`` flag
implies ``--ignore-promote-rules``. However, rules that promote only
a subset of their targets via ``(only ...)`` are never ignored.

Inferred Rules
~~~~~~~~~~~~~~

When using the action DSL (see :ref:`user-actions`), the dependencies
and targets are usually obvious.

For instance:

.. code:: lisp

    (rule
     (target b)
     (deps   a)
     (action (copy %{deps} %{target})))

In this example, the dependencies and targets are obvious by inspecting
the action. When this is the case, you can use the
following shorter syntax and have Dune infer dependencies and
targets for you:

.. code:: scheme

    (rule <action>)

For instance:

.. code:: scheme

    (rule (copy a b))

Note that in Dune, targets must always be known
statically. For instance, this ``(rule ...)``
stanza is rejected by Dune:

.. code:: lisp

    (rule (copy a b.%{read:file}))

Directory targets
-----------------

Note that at this time, Dune officially only supports user rules with targets in
the current directory. However, starting from Dune 3.0, we provide an
experimental support for *directory targets*, where an action can produce a
whole tree of build artifacts. To specify a directory target, you can use the
``(dir <dirname>)`` syntax. For example, the following stanza describes a rule
with a file target ``foo`` and a directory target ``bar``.

.. code:: scheme

    (rule
     (targets foo (dir bar))
     (action  <action>))

To enable this experimental feature, add ``(using directory-targets 0.1)`` to
your ``dune-project`` file. However note that currently rules with a directory
target are always rebuilt. We are working on fixing this performance bug.

ocamllex
--------

``(ocamllex <names>)`` is essentially a shorthand for:

.. code:: lisp

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

.. code:: lisp

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

A ``menhir`` stanza is available to support the Menhir parser generator.

To use Menhir in a Dune project, the language version should be selected in the
``dune-project`` file. For example:

.. code:: scheme

  (using menhir 2.0)

This will enable support for Menhir stanzas in the current project. If the
language version is absent, Dune will automatically add this line with the
latest Menhir version once a Menhir stanza is used anywhere.

The basic form for defining menhir-git_ parsers (analogous to :ref:`ocamlyacc`) is:

.. code:: scheme

    (menhir
     (modules <parser1> <parser2> ...)
     <optional-fields>)

``<optional-fields>`` are:

- ``(merge_into <base_name>)`` is used to define modular parsers. This
  correspond to the ``--base`` command line option of ``menhir``. With this
  option, a single parser named ``base_name`` is generated.

- ``(flags <option1> <option2> ...)`` is used to pass extra flags to Menhir.

- ``(infer <bool>)`` is used to enable Menhir with type
  inference. This option is enabled by default with Menhir language 2.0.

Menhir supports writing the grammar and automation to the ``.cmly`` file. Therefore,
if this is flag is passed to Menhir, Dune will know to introduce a ``.cmly``
target for the module.

.. _menhir-git: https://gitlab.inria.fr/fpottier/menhir


cinaps
------

A ``cinaps`` stanza is available to support the ``cinaps`` tool.  See
the `cinaps website <https://github.com/janestreet/cinaps>`_ for more
details.

.. _documentation-stanza:

documentation
-------------

Additional manual pages may be attached to packages using the ``documentation``
stanza. These ``.mld`` files must contain text in the same syntax as OCamldoc
comments.

.. code-block:: scheme

  (documentation (<optional-fields>))

Where ``<optional-fields>`` are:

- ``(package <name>)`` defines the package this documentation should be attached to. If
  this is absent, Dune will try to infer it based on the location of the
  stanza.

- ``(mld_files <arg>)``: the ``<arg>`` field follows the
  :ref:`ordered-set-language`. This is a set of extensionless MLD file basenames
  attached to the package, where ``:standard`` refers to all the
  ``.mld`` files in the stanza's directory.

For more information, see :ref:`documentation`.

.. _alias-stanza:

alias
-----

The ``alias`` stanza adds dependencies to an alias or specifies an action
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

- ``<action>``, an action for constructing the alias. See the
  :ref:`user-actions` section for more details. Note that this is removed in Dune
  2.0, so users must port their code to use the
  ``rule`` stanza with the ``alias`` field instead.

- ``(package <name>)`` indicates that this alias stanza is part of package
  ``<name>`` and should be filtered out if ``<name>`` is filtered out from the
  command line, either with ``--only-packages <pkgs>`` or ``-p <pkgs>``.

- ``(locks (<lock-names>))`` specifies that the action must be run while
  holding the following locks. See the :ref:`locks` section for more details.

- ``(enabled_if <blang expression>)`` specifies the Boolean condition that must
  be true for the tests to run. The condition is specified using the :ref:`blang`, and
  the field allows for :ref:`variables` to appear in the expressions.

The typical use of the ``alias`` stanza is to define tests:

.. code:: lisp

    (rule
     (alias   runtest)
     (action (run %{exe:my-test-program.exe} blah)))

See the section about :ref:`running-tests` for details.

Please note: if your project contains several packages, and you run the tests
from the opam file using a ``build-test`` field, all your ``runtest`` alias
stanzas should have a ``(package ...)`` field in order to partition the set of
tests.

.. _install:

install
-------

Dune supports installing packages on the system, i.e., copying freshly built
artifacts from the workspace to the system. The ``install`` stanza takes three
pieces of information:

- the list of files to install
- the package to attach these files. (This field is optional if your
  project contains a single package.)
- the section in which the files will be installed

For instance:

.. code::

   (install
    (files hello.txt)
    (section share)
    (package mypackage))

Indicate that the file ``hello.txt`` in the current directory is to be
installed in ``<prefix>/share/mypackage``.

The following sections are available:

- ``lib`` installs by default to ``<prefix>/lib/<pkgname>/``
- ``lib_root`` installs by default to ``<prefix>/lib/``
- ``libexec`` installs by default to ``<prefix>/lib/<pkgname>/`` with the
  executable bit set
- ``libexec_root`` installs by default to ``<prefix>/lib/`` with the executable
  bit set
- ``bin`` installs by default  to ``<prefix>/bin/`` with the executable bit set
- ``sbin`` installs by default  to ``<prefix>/sbin/`` with the executable bit set
- ``toplevel`` installs by default  to ``<prefix>/lib/toplevel/``
- ``share`` installs by default  to ``<prefix>/share/<pkgname>/``
- ``share_root`` installs by default  to ``<prefix>/share/``
- ``etc`` installs by default  to ``<prefix>/etc/<pkgname>/``
- ``doc`` installs by default  to ``<prefix>/doc/<pkgname>/``
- ``stublibs`` installs by default  to ``<prefix>/lib/stublibs/`` with the
  executable bit set
- ``man`` installs by default  relative to ``<prefix>/man`` with the destination
  directory extracted from the extension of the source file (so that
  installing ``foo.1`` is equivalent to a destination of
  ``man1/foo.1``)
- ``misc`` requires files to specify an absolute destination. It will only work
  when used with opam and the user will be prompted before the installation when
  it's done via opam. It is deprecated.
- ``(site (<package> <site>))`` installs in the ``<site>`` directory of
  ``<package>``. If the prefix isn't the same as the one used when installing
  ``<package>``, ``<package>`` won't find the files.

Normally, Dune uses the file's basename to determine
the file's name once installed; however, you can change that
by using the form ``(<filename> as <destination>)`` in the
``files`` field. For instance, to install a file ``mylib.el`` as
``<prefix>/emacs/site-lisp/mylib.el``, you must write the following:

.. code:: scheme

    (install
     (section share_root)
     (files   (mylib.el as emacs/site-lisp/mylib.el)))


Handling of the .exe Extension on Windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under Microsoft Windows, executables must be suffixed with
``.exe``. Dune tries to ensure that executables are always
installed with this extension on Windows.

More precisely, when installing a file via an ``(install ...)``
stanza, Dune implicitly adds the ``.exe`` extension to the destination,
if the source file has extension ``.exe`` or ``.bc`` and if it's not
already present

copy_files
----------

The ``copy_files`` and ``copy_files#`` stanzas specify that
files from another directory could be copied to the current
directory, if needed.

The syntax is as follows:

.. code:: scheme

    (copy_files
     <optional-fields>
     (files <glob>))

``<glob>`` represents the set of files to copy. See the :ref:`glob
<glob>` for details.

``<optional-fields>`` are:

- ``(alias <alias-name>)`` is used to specify an alias to which to attach the targets.

- ``(mode <mode>)`` is used to specify how to handle the targets. See `modes`_
  for details.

- ``(enabled_if <blang expression>)`` conditionally disables this stanza. The
  condition is specified using the :ref:`blang`.

The short form

.. code:: scheme

    (copy_files <glob>)

is equivalent to

.. code:: scheme

    (copy_files (files <glob>))

The difference between ``copy_files`` and ``copy_files#`` is the same
as the difference between the ``copy`` and ``copy#`` actions. See the
:ref:`user-actions` section for more details.

include
-------

The ``include`` stanza allows including the contents of another file in the
current ``dune`` file. Currently, the included file cannot be generated and must be
present in the source tree. This feature is intended for use in conjunction
with promotion, when parts of a ``dune`` file are to be generated.

For instance:

.. code:: scheme

    (include dune.inc)

    (rule (with-stdout-to dune.inc.gen (run ./gen-dune.exe)))

    (rule
     (alias  runtest)
     (action (diff dune.inc dune.inc.gen)))

With this ``dune`` file, running Dune as follows will replace the
``dune.inc`` file in the source tree by the generated one:

.. code:: shell

    $ dune build @runtest --auto-promote

.. _tests-stanza:

tests
-----

The ``tests`` stanza allows one to easily define multiple tests. For example, we
can define two tests at once with:

.. code:: scheme

   (tests
    (names mytest expect_test)
    <optional fields>)

This defines an executable named ``mytest.exe`` that will be executed as
part of the ``runtest`` alias. If the directory also contains an
``expect_test.expected`` file, then ``expect_test`` will be used to define an
expect test. That is, the test will be executed and its output will be compared
to ``expect_test.expected``.

The optional fields supported are a subset of the alias and executables
fields. In particular, all fields except for ``public_names`` are supported from
the :ref:`executables stanza <shared-exe-fields>`. Alias fields apart from
``name`` are allowed.

By default, the test binaries are run without options.  The ``action`` field can
override the test binary invocation, i.e., if you're using
alcotest and wish to see all the test failures on the standard output. When
running Dune ``runtest`` you can use the following stanza:

.. code:: lisp

   (tests
    (names mytest)
    (libraries alcotest mylib)
    (action (run %{test} -e)))

Starting from Dune 2.9, it's possible to automatically generate empty interface
files for test executables. See `executables_implicit_empty_intf`_.

test
----

The ``test`` stanza is the singular form of ``tests``. The only difference is
that it's of the form:

.. code:: scheme

   (test
    (name foo)
    <optional fields>)

The ``name`` field is singular, and the same optional fields are supported.

.. _dune-env:

env
---

The ``env`` stanza allows one to modify the environment. The syntax is as
follows:

.. code:: scheme

     (env
      (<profile1> <settings1>)
      (<profile2> <settings2>)
      ...
      (<profilen> <settingsn>))

The first form ``(<profile> <settings>)`` that corresponds to the
selected build profile will be used to modify the environment in this
directory. You can use ``_`` to match any build profile.

Fields supported in ``<settings>`` are:

- any OCaml flags field. See :ref:`ocaml-flags` for more details.

- ``(link_flags <flags>)`` to specify flags to ocaml when linking an
  executable. See :ref:`executables stanza <shared-exe-fields>`.

- ``(c_flags <flags>)`` and ``(cxx_flags <flags>)``
  to specify compilation flags for C and C++ stubs, respectively.
  See `library`_ for more details.

- ``(env-vars (<var1> <val1>) .. (<varN> <valN>))``, which will add the
  corresponding variables to the environment in which the build commands are
  executed and under which ``dune exec`` runs.

- ``(menhir_flags <flags>))`` specifies flags for Menhir stanzas.

- ``(js_of_ocaml (flags <flags>)(build_runtime <flags>)(link_flags <flags>))``
  to specify js_of_ocaml flags. see `jsoo-field`_ for more details.

- ``(js_of_ocaml (compilation_mode <mode>))``, where ``<mode>`` is
  either ``whole_program`` or ``separate``. This field controls
  whether to use separate compilation or not.

- ``(js_of_ocaml (runtest_alias <alias-name>))`` is used to specify
  the alias under which :ref:`inline_tests` and tests (`tests-stanza`_)
  run for the `js` mode.

- ``(binaries <binaries>)``, where ``<binaries>`` is a list of entries
  of the form ``(<filepath> as <name>)``. ``(<filepath> as <name>)``
  makes the binary ``<filepath>`` available in the command search as
  just ``<name>``. For instance, in a ``(run <name> ...)`` action,
  ``<name>`` will resolve to this file path. You can also write just
  the file path, in which case the name will be inferred from the
  basename of ``<filepath>`` by dropping the ``.exe`` suffix, if it
  exists. For example, ``(binaries bin/foo.exe (bin/main.exe as
  bar))`` would add the commands ``foo`` and ``bar`` to the search
  path.

- ``(inline_tests <state>)``, where ``<state>`` is either ``enabled``, ``disabled``, or
  ``ignored``. This field has been available since Dune 1.11. It controls the variable's value
  ``%{inline_tests}``, which is read by the inline test framework.
  The default value is ``disabled`` for the ``release`` profile and ``enabled``
  otherwise.

- ``(odoc <fields>)`` allows passing options to Odoc. See
  :ref:`odoc-options` for more details.

- ``(coq (flags <flags>))`` allows passing options to Coq. See
  :ref:`coq-theory` for more details.

- ``(formatting <settings>)`` allows the user to set auto-formatting in the current
  directory subtree (see :ref:`formatting`).

.. _dune-subdirs:

dirs (Since 1.6)
----------------

The ``dirs`` stanza allows specifying the subdirectories Dune will
include in a build. The syntax is based on Dune's :ref:`predicate-lang` and allows
the user the following operations:

- The special value ``:standard`` which refers to the default set of used
  directories. These are the directories that don't start with ``.`` or ``_``.

- Set operations. Differences are expressed with backslash: ``* \ bar``; unions
  are done by listing multiple items.

- Sets can be defined using globs.

Examples:

.. code:: lisp

   (dirs *) ;; include all directories
   (dirs :standard \ ocaml) ;; include all directories except ocaml
   (dirs :standard \ test* foo*) ;; exclude all directories that start with test or foo

Dune will not scan a directory that isn't included in this stanza.
Any contained Dune (or other special) files won't be interpreted either and
will be treated as raw data. It is however possible to depend on files inside
ignored subdirectories.

.. _dune-data_only_dirs:

data_only_dirs (Since 1.6)
--------------------------

Dune allows the user to treat directories as *data only*. ``dune`` files in these
directories won't be evaluated for their rules, but the contents of these
directories will still be usable as dependencies for other rules.

The syntax is the same as for the ``dirs`` stanza except that ``:standard``
is empty by default.

Example:

.. code:: scheme

   ;; dune files in fixtures_* dirs are ignored
   (data_only_dirs fixtures_*)

.. _dune-ignored_subdirs:

ignored_subdirs (Deprecated in 1.6)
-----------------------------------

One may also specify *data only* directories using the ``ignored_subdirs``
stanza, meaning it's the same as ``data_only_dirs``, but the syntax isn't as
flexible and only accepts a list of directory names. It's advised to switch to
the new ``data_only_dirs`` stanza.

Example:

.. code:: scheme

     (ignored_subdirs (<sub-dir1> <sub-dir2> ...))

All of the specified ``<sub-dirn>`` will be ignored by Dune. Note that users
should rely on the ``dirs`` stanza along with the appropriate set operations
instead of this stanza. For example:

.. code:: lisp

  (dirs :standard \ <sub-dir1> <sub-dir2> ...)

.. _dune-vendored_dirs:

vendored_dirs (Since 1.11)
--------------------------

Dune supports vendoring other Dune-based projects natively, since simply
copying a project into a subdirectory of your own project will work. Simply
doing that has a few limitations though. You can workaround those by explicitly
marking such directories as containing vendored code.

Example:

.. code:: scheme

   (vendored_dirs vendor)


Dune will not resolve aliases in vendored directories. By default, it won't
build all installable targets, run the tests, format, or lint the code located
in such a directory while still building your project's dependencies.
Libraries and executables in vendored directories will also be built with a ``-w
-a`` flag to suppress all warnings and prevent pollution of your build output.


.. _include_subdirs:

include_subdirs
---------------

The ``include_subdirs`` stanza is used to control how Dune considers
subdirectories of the current directory. The syntax is as follows:

.. code:: scheme

     (include_subdirs <mode>)

Where ``<mode>`` maybe be one of:

- ``no``, the default
- ``unqualified``

When the ``include_subdirs`` stanza isn't present or ``<mode>`` is
``no``, Dune considers subdirectories independent. When ``<mode>``
is ``unqualified``, Dune will assume that the current directory's
subdirectories are part of the same group of directories. In
particular, Dune will simultaneously scan all these directories when looking
for OCaml/Reason files. This allows you to split a library between
several directories. ``unqualified`` means that modules in
subdirectories are seen as if they were all in the same directory. In
particular, you cannot have two modules with the same name in two
different directories. We plan to add a ``qualified`` mode in
the future.

Note that subdirectories are included recursively, however the
recursion will stop when encountering a subdirectory that contains
another ``include_subdirs`` stanza. Additionally, it's not allowed
for a subdirectory of a directory with ``(include_subdirs <x>)``
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

``(preprocess (pps ...))`` is the same as the ``(preprocess (pps ...))`` field
of `library`_. Currently, ``action`` and ``future_syntax`` are not supported
in the toplevel.

.. _subdir:

subdir
------

The ``subdir`` stanza can be used to evaluate stanzas in sub directories. This is
useful for generated files or to override stanzas in vendored directories
without editing vendored ``dune`` files.

In this example, a ``bar`` target is created in the ``foo`` directory, and a bar
target will be created in ``a/b/bar``:

.. code:: scheme

   (subdir foo (rule (with-stdout-to bar (echo baz))))
   (subdir a/b (rule (with-stdout-to bar (echo baz))))

coq.theory
~~~~~~~~~~

See the documentation on the :ref:`coq-theory`, :ref:`coq-extraction`,
:ref:`coq-pp`, and related stanzas.


external_variant
-----------------

This stanza was experimental and removed in Dune 2.6. See :ref:`dune-variants`.

MDX (Since 2.4)
---------------

MDX is a tool that helps you keep your markdown documentation up-to-date by
checking that its code examples are correct. When setting an MDX
stanza, the checks MDX carries out are automatically attached to the
``runtest`` alias of the stanza's directory.

See `MDX's repository <https://github.com/realworldocaml/mdx>`__ for more details.

You can define an MDX stanza to specify which files you want checked.

Note that this feature is still experimental and needs to be enabled in your
``dune-project`` with the following ``using`` stanza:

.. code:: scheme

  (using mdx 0.2)

.. note:: Version ``0.2`` of the stanza requires mdx ``1.9.0``.


The syntax is as follows:

.. code:: scheme

  (mdx <optional-fields>)

Where ``<optional-fields>`` are:

- ``(files <globs>)`` are the files that you want MDX to check, described as a
  list of globs (see the :ref:`Glob language specification <glob>` ).
  It defaults to ``*.md``.

- ``(deps <deps-conf list>)`` to specify the dependencies
  of your documentation code blocks. See the :ref:`deps-field` section for more
  details.

- ``(preludes <files>)`` are the prelude files you want to pass to MDX.
  See `MDX's documentation <https://github.com/realworldocaml/mdx>`__ for more
  details on preludes.

- ``(libraries <libraries>)`` are libraries that should be
  statically linked in the MDX test executable.

- ``(enabled_if <blang expression>)``  is the same as the corresponding field
  of `library`_.

- ``(package <package>)`` specifies which package to attach this stanza to
  (similarly to when ``(package)`` is attached to a ``(rule)`` stanza). When
  ``-p`` is passed, ``(mdx)`` stanzas with another package will be ignored.
  Note that this feature is completely separate from ``(packages)``, which
  specifies some dependencies.

- ``(locks <lock-names>)`` specifies that the action of running the tests
  holds the specified locks.  See the :ref:`locks` section for more details.

Upgrading from Version 0.1
~~~~~~~~~~~~~~~~~~~~~~~~~~

- The 0.2 version of the stanza requires at least MDX 1.9.0. If you encounter
  an error such as, ``ocaml-mdx: unknown command `dune-gen'``, then you should
  upgrade MDX.

- The field ``(packages <packages>)`` is deprecated in version 0.2. You can
  use package items in the generic ``deps`` field instead:
  ``(deps (package <package>) ... (package <package>))``

- Use the new ``libraries`` field to directly link libraries in the test
  executable and remove the need for ``#require`` directives in your
  documentation code blocks.

.. _plugin:

plugin (Since 2.8)
------------------

Plugins are a way to load OCaml libraries at runtime. The ``plugin`` stanza
allows you to declare the plugin's name, in which :ref:`sites` should be
present, and which libraries it will load.

.. code:: lisp

   (plugin
    (name <name>)
    (libraries <libraries>)
    (site (<package> <site name>))
    (<optional-fields>))

``<optional-fields>`` are:

- ``(package <package>)`` if there are more than one package defined in the
  current scope, this specifies which package the
  plugin will install. A plugin can be installed by one package in the site
  of another package.

- ``(optional)`` will not declare the plugin if the libraries are not available.

The loading of the plugin is done using the facilities generated by
:ref:`generate_sites_module`.

.. _generate_sites_module:

generate_sites_module (Since 2.8)
---------------------------------

Dune proposes some facilities for dealing with :ref:`sites` in a program. The
``generate_sites_module`` stanza will generate code for looking up the correct locations
of the sites directories and for loading plugins. It works after installation
with or without the relocation mode, inside Dune rules, when using Dune executables.
For promotion, it works only if the generated modules are solely in the executable (or
library statically linked) promoted; generated modules in plugins won't work.

.. code:: lisp

   (generate_sites_module
    (module <name>)
    <facilities>)

The module's code is generated in the directory with the given name. The
code is populated according to the requested facilities.


The available ``<facilities>`` are:

- ``sourceroot`` : adds in the generated module a value ``val sourceroot: string option``,
  which contains the value of ``%{workspace_root}``, if the code have been built
  locally. It could be used to keep the tool's configuration file locally when
  executed with ``dune exec`` or after promotion. The value is ``None`` once it has been installed.

- ``relocatable`` : adds in the generated module a value ``val relocatable: bool``,
  which indicates if the binary has been installed in the relocatable mode

- ``(sites <package>)`` : adds in the submodule `Sites` of the generated module a value
  ``val <site>: string list`` for each ``<site>`` of ``<package>``. The
  identifier <site> isn't capitalized.

- ``(plugins (<package> <site>) ...)``: adds in the submodule ``Plugins`` of the
  generated module a submodule ``<site>`` with the following signature ``S``. The
  identifier ``<site>`` is capitalized.

.. code:: ocaml

   module type S = sig
     val paths: string list
     (** return the locations of the directory containing the plugins *)

     val list: unit -> string list
     (** return the list of available plugins *)

     val load_all: unit -> unit
     (** load all the plugins and their dependencies *)

     val load: string -> unit
     (** load the specified plugin and its dependencies *)
   end

The generated module is a dependency on the library ``dune-site``,
and if the facilities ``(plugins ...)`` are used, it is a dependency on the library
``dune-site.plugins``. Those dependencies are not automatically added
to the library or executable which use the module (cf. :ref:`plugins`).

.. _dune-workspace:

dune-workspace
==============

By default, a workspace has only one build context named ``default`` which
corresponds to the environment in which ``dune`` is run. You can define more
contexts by writing a ``dune-workspace`` file.

You can point Dune to an explicit ``dune-workspace`` file with the
``--workspace`` option. For instance, it's good practice to write a
``dune-workspace.dev`` in your project with all the OCaml versions your
projects' support, so developers can test that the code builds with all
OCaml versions by simply running:

.. code:: bash

    $ dune build --workspace dune-workspace.dev @all @runtest

The ``dune-workspace`` file uses the S-expression syntax. This is what
a typical ``dune-workspace`` file looks like:

.. code:: scheme

    (lang dune 3.4)
    (context (opam (switch 4.07.1)))
    (context (opam (switch 4.08.1)))
    (context (opam (switch 4.11.1)))

The rest of this section describe the stanzas available.

Note that an empty ``dune-workspace`` file is interpreted the same as one
containing exactly:

.. code:: scheme

    (lang dune 3.2)
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
stanzas. The syntax for this stanza is the same as Dune's :ref:`dune-env` stanza.

context
-------

The ``(context ...)`` stanza declares a build context. The argument
can be either ``default`` or ``(default)`` for the default build
context, or it can be the description of an opam switch, as follows:

.. code:: scheme

    (context (opam (switch <opam-switch-name>)
                   <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the subdirectory's name for ``_build``,
   where this build's context artifacts will be stored.

-  ``(root <opam-root>)`` is the opam root. By default, it will take
   the opam root defined by the environment in which ``dune`` is
   run, which is usually ``~/.opam``.

- ``(merlin)`` instructs Dune to use this build context for
  Merlin.

- ``(profile <profile>)`` sets a different profile for a build
  context. This has precedence over the command-line option
  ``--profile``.

- ``(env <env>)`` sets the environment for a particular context. This is of
  higher precedence than the root ``env`` stanza in the workspace file. This
  field has the same options as the :ref:`dune-env` stanza.

- ``(toolchain <findlib_toolchain>)`` sets a ``findlib`` toolchain for the context.

- ``(host <host_context>)`` chooses a different context to build binaries that
  are meant to be executed on the host machine, such as preprocessors.

- ``(paths (<var1> <val1>) .. (<varN> <valN>))`` allows you to set the value of any
  ``PATH``-like variables in this context. If ``PATH`` itself is modified in
  this way, its value will be used to resolve workspace binaries,
  including finding the compiler and related tools. These variables will also be
  passed as part of the environment to any program launched by Dune. For
  each variable, the value is specified using the :ref:`ordered-set-language`.
  Relative paths are interpreted with respect to the workspace root. See
  :ref:`finding-root`.

- ``(fdo <target_exe>)`` builds this context with feedback-direct
  optimizations. It requires `OCamlFDO
  <https://github.com/gretay-js/ocamlfdo>`__. ``<target_exe>`` is a
  path-interpreted relative to the workspace root (see
  :ref:`finding-root`). ``<target_exe>`` specifies which executable to
  optimize. Users should define a different context for each target
  executable built with FDO. The context name is derived
  automatically from the default name and ``<target-exe>``, unless
  explicitly specified using the ``(name ...)`` field.  For example, if
  ``<target_exe>`` is *src/foo.exe* in a default context, then the
  name of the context is *default-fdo-foo* and the filename
  that contains execution counters is *src/fdo.exe.fdo-profile*.  This
  feature is **experimental** and no backwards compatibility is
  implied.

- By default, Dune builds and installs dynamically-linked foreign
  archives (usually named ``dll*.so``). It's possible to disable
  this by setting by including
  ``(disable_dynamically_linked_foreign_archives true)`` in the
  workspace file, so bytecode executables will be built
  with all foreign archives statically linked into the runtime system.


Both ``(default ...)`` and ``(opam ...)`` accept a ``targets`` field in order to
setup cross compilation. See :ref:`cross-compilation` for more
information.

Merlin reads compilation artifacts, and it can only read the compilation
artifacts of a single context. Usually, you should use the artifacts from the
``default`` context, and if you have the ``(context default)`` stanza in your
``dune-workspace`` file, that is the one Dune will use.

For rare cases where this is not what you want, you can force Dune to use a
different build contexts for Merlin by adding the field ``(merlin)`` to this
context.
