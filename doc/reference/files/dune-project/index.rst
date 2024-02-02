dune-project
============

These files are used to mark the root of projects as well as define project-wide
parameters. The first line of ``dune-project`` must be a ``lang`` stanza with no
extra whitespace or comments. The ``lang`` stanza controls the names and
contents of all configuration files read by Dune and looks like:

.. code:: dune

   (lang dune 3.14)

Additionally, they can contains the following stanzas.

accept_alternative_dune_file_name
---------------------------------

.. dune:stanza:: accept_alternative_dune_file_name

   .. versionadded:: 3.0

   Specify that the alternative filename ``dune-file`` is accepted in addition
   to ``dune``.

   This may be useful to avoid problems with ``dune`` files that have the
   executable permission in a directory in the ``PATH``, which can unwittingly
   happen on Windows.

   Note that ``dune`` continues to be accepted even after enabling this option,
   but if a file named ``dune-file`` is found in a directory, it will take
   precedence over ``dune``.

cram
----

.. dune:stanza:: cram
   :param: <status>

   Define whether Cram-style tests are enabled for the project.

   `<status>` can be either ``enable`` or ``disable``. The default is
   ``enable`` starting from the language version 3.0.

   .. seealso:: :ref:`cram-tests`

.. _dialect:

dialect
-------

.. dune:stanza:: dialect

   Declare a new :term:`dialect`.

   .. dune:field:: name
      :param: <name>

      The name of the dialect being defined. It must be unique in a given
      project.

      This field is required.

   .. dune:field:: implementation

      Details related to the implementation files (corresponding to `*.ml`).

      .. versionchanged:: 3.9 This field is made optional.

      .. dune:field:: extension
         :param: <string>

         Specify the file extension used for this dialect.

         The extension string must not start with a period and be unique in a
         given project (so that a given extension can be mapped back to a
         corresponding dialect). In Dune 3.9 and later, the extension string may
         contain periods (e.g., `cppo.ml`).

         This field is required.

      .. dune:field:: preprocess
         :param: <action>

         Run `<action>` to produce a valid OCaml abstract syntax tree.

         This action is expected to read the file given in the variable named
         ``%{input-file}`` and output a *binary* abstract syntax tree on its
         standard output.

         If the field is not present, it is assumed that the corresponding
         source code is already valid OCaml code and can be passed to the OCaml
         compiler as-is.

         .. seealso:: :ref:`preprocessing-actions`

      .. dune:field:: format
         :param: <action>

         Run `<action>` to format source code for this dialect.

         The action is expected to read the file given in the variable named
         ``%{input-file}`` and output the formatted source code on its standard
         output.

         If the field is not present, the behaviour depends on the presence of
         ``(preprocess)``: if it is also not present (that is, the dialect
         consists of valid OCaml code), then the dialect will be formatted as
         any other OCaml code. Otherwise no special formatting will be done.

         .. seealso:: :doc:`/howto/formatting`

   .. dune:field:: interface

      Details related to the interface files (corresponding to `*.mli`).

      This field supports the same sub-fields as ``implementation``.

      .. versionchanged:: 3.9 This field is made optional.

.. _executables_implicit_empty_intf:

executables_implicit_empty_intf
-------------------------------

.. dune:stanza:: executables_implicit_empty_intf

   .. versionadded:: 2.9

   Automatically generate empty interface files for executables and tests that
   do not already have them.

   By default, executables defined via ``(executables(s) ...)`` or ``(test(s)
   ...)`` stanzas are compiled with the interface file provided (e.g., ``.mli``
   or ``rei``). Since these modules cannot be used as library dependencies,
   it is common to give them empty interface files to strengthen the compiler's
   ability to detect unused values in these modules.

   This option, when enabled, will generate an empty `*.mli` file.

   Example:

   .. code:: dune

       (executables_implicit_empty_intf true)

   This option is enabled by default starting with Dune lang 3.0.

expand_aliases_in_sandbox
-------------------------

.. dune:stanza:: expand_aliases_in_sandbox

   When a sandboxed action depends on an alias, copy the expansion of the alias
   inside the sandbox. For instance, in the following example:

   .. code:: dune

      (alias
       (name foo)
       (deps ../x))

      (cram
       (deps (alias foo)))

   File `x` will be visible inside the Cram test if and only if this option is
   enabled. This option is a better default in general; however, it currently
   causes Cram tests to run noticeably slower. So it is disabled by default
   until the performance issue with Cram test is fixed.

.. _explicit-js-mode:

explicit_js_mode
----------------

.. dune:stanza:: explicit_js_mode

   Do not implicitly add ``js`` to the ``(modes ...)`` field of executables.

   In projects that use dune lang 1.x, JavaScript targets are defined for every
   bytecode executable. This is not very precise and does not interact well
   with the ``@all`` alias.

   It is possible to opt out of this behavior by using:

   .. code:: dune

       (explicit_js_mode)

   When this is enabled, an explicit ``js`` mode needs to be added to the
   ``(modes ...)`` field of executables in order to trigger the JavaScript
   compilation. Explicit JS targets declared like this will be attached to the
   ``@all`` alias.

   Starting with Dune 2.0, this behavior is the default, and there is no way to
   disable it.

.. _formatting:

formatting
----------

.. dune:stanza:: formatting

   .. versionadded:: 2.0

   Control automatic formatting. Several forms are accepted:

   - To disable automatic formatting completely (equivalent to the behaviour in
     language 1.x):

     .. code:: dune

        (formatting disabled)

   - To restrict the languages that are considered for formatting:

     .. code:: dune

        (formatting
         (enabled_for <languages>))

     The list of `<languages>` can be either ``dune`` (formatting of dune
     files) or a :term:`dialect` name.

   .. seealso:: :doc:`/howto/formatting`

.. _generate_opam_files:

generate_opam_files
-------------------

.. dune:stanza:: generate_opam_files

   Use metadata specified in the ``dune-project`` file to generate ``.opam``
   files.

   To enable this integration, add the following field to the ``dune-project``
   file:

   .. code:: dune

      (generate_opam_files)

   .. seealso:: :doc:`/howto/opam-file-generation`

Dune uses the following global fields to set the metadata for all packages
defined in the project:

.. dune:stanza:: license
   :param: <strings>

   Specify the license of the project, ideally as an identifier from the `SPDX
   License List <https://spdx.org/licenses/>`__.

   Example:

   .. code:: dune

      (license MIT)

   Multiple licenses may be specified.

.. dune:stanza:: authors
   :param: <strings>

   Specify authors.

   Example:

   .. code:: dune

      (authors
       "Jane Doe <jane.doe@example.com>"
       "John Doe <john.doe@example.com>")

.. dune:stanza:: maintainers
   :param: <strings>

   Specify maintainers.

   Example:

   .. code:: dune

      (maintainers
       "Jane Doe <jane.doe@example.com>"
       "John Doe <john.doe@example.com>")

.. dune:stanza:: source

   Specify where the source for the package can be found.

   It can be specified as ``(uri <uri>)`` or using shortcuts for some
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

   Examples:

   .. code:: dune

      (source
       (github ocaml/dune))

   .. code:: dune

      (source
       (uri https://dev.example.com/project.git))

.. dune:stanza:: bug_reports
   :param: <url>

   Where bugs should be reported.

   If a hosting service is used in ``(source)``, a default value is provided.

   Example:

   .. code:: dune

      (bug_reports https://dev.example.com/project/issues)

.. dune:stanza:: homepage
   :param: <url>

   The homepage of the project.

   If a hosting service is used in ``(source)``, a default value is provided.

   Example:

   .. code:: dune

      (bug_reports https://example.com/)

.. dune:stanza:: documentation
   :param: <url>

   Where the documentation is hosted.

With these fields, every time one calls Dune to execute some rules (either via
``dune build``, ``dune runtest``, or something else), the opam files get
generated.

Some or all of these fields may be overridden for each package of the project,
see :ref:`package`.

.. _implicit_transitive_deps:

implicit_transitive_deps
------------------------

.. dune:stanza:: implicit_transitive_deps

   Control whether transitive dependencies are made implicitly visible.

   By default, Dune allows transitive dependencies of dependencies used when
   compiling OCaml. However, this can be disabled by specifying:

   .. code:: dune

       (implicit_transitive_deps false)

   Then all dependencies directly used by a library or an executable must be
   added in the ``libraries`` field.

   We recommend users experiment with this mode and report any problems.

   Note that you must use ``threads.posix`` instead of ``threads`` when using
   this mode. This isn't an important limitation, as ``threads.vm`` is
   deprecated anyway.

   In some situations, it can be desirable to selectively preserve the behavior
   of transitive dependencies' availability a library's users. For example, if
   we define a library ``foo_more`` that extends ``foo``, we might want
   ``foo_more`` users to immediately have ``foo`` available as well. To do
   this, we must define the dependency on ``foo`` as re-exported:

   .. code:: dune

      (library
       (name foo_more)
       (libraries (re_export foo)))

name
----

.. dune:stanza:: name
   :param: <string>

   Set the name of the project.

   It is used by :ref:`dune subst <dune-subst>` and error messages.

opam_file_location
------------------

.. dune:stanza:: opam_file_location
   :param: <location>

   .. versionadded:: 3.8

   Configure where generated ``.opam`` files are located. `<location>` can
   be one of the following:

   - ``relative_to_project``: the ``.opam`` files are generated in the project
     root directory. This is the default.

   - ``inside_opam_directory``: the ``.opam`` files are generated in a directory
     named ``opam`` in the project root directory.

   .. seealso:: :doc:`/howto/opam-file-generation`

.. _package:

package
-------

.. dune:stanza:: package

   Define package-specific metadata.

   .. dune:field:: name
      :param: <string>

      The name of the package.

      This must be specified.

   .. dune:field:: synopsis
      :param: <string>

      A short package description.

   .. dune:field:: description
      :param: <string>

      A longer package description.

   .. dune:field:: depends
      :param: <dep-specification>

      Package dependencies, as :token:`~pkg-dep:dep_specification`.

   .. dune:field:: conflicts
      :param: <dep-specification>

      Package conflicts, as :token:`~pkg-dep:dep_specification`.

   .. dune:field:: depopts
      :param: <dep-specification>

      Optional package dependencies, as :token:`~pkg-dep:dep_specification`.

   .. dune:field:: tags
      :param: <tags>

      A list of tags.

   .. dune:field:: deprecated_package_names
      :param: <name list>

      A list of names that can be used with the :ref:`deprecated-library-name`
      stanza to migrate legacy libraries from other build systems that do not
      follow Dune's convention of prefixing the library's public name with the
      package name.

   .. dune:field:: license

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: authors

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: maintainers

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: source

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: bug_reports

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: homepage

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: documentation

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: sites

      Define a site.

      ``(sites (<section> <name>) ...)`` defines a site named ``<name>`` in the
      section ``<section>``.

Adding libraries to different packages is done via the ``public_name`` and
``package`` fields. See :ref:`library` section for details.

The list of dependencies :token:`~pkg-dep:dep_specification` is modelled after
opam's own language. The syntax is a list of the following elements:

.. productionlist:: pkg-dep
   op : '=' | '<' | '>' | '<>' | '>=' | '<='
   filter : :dev | :build | :with-test | :with-doc | :post
   constr : (<op> <version>)
   logop : or | and
   dep : <name>
       : (<name> <filter>)
       : (<name> <constr>)
       : (<name> (<logop> (<filter> | <constr>))*)
   dep_specification : <dep>+

Filters will expand to any opam variable name if prefixed by ``:``, not just the
ones listed in :token:`~pkg-dep:filter`. This also applies to version numbers.
For example, to generate ``depends: [ pkg { = version } ]``, use ``(depends
(pkg (= :version)))``.

Note that the use of a ``using`` stanza (see :ref:`using <using>`) doesn't
automatically add the associated library or tool as a dependency. They have to
be added explicitly.

.. _subst:

subst
-----

.. dune:stanza:: subst
   :param: <bool>

   .. versionadded: 3.0

   Control whether :ref:`dune-subst` is enabled for this project.

   - ``(subst disabled)``, means that any call of ``dune subst`` in this
     project is forbidden and will result in an error. This line will be
     omitted from the build instructions when generating opam files.
   - ``(subst enabled)`` allows substitutions explicitly. This is the default.

.. _always-add-cflags:

use_standard_c_and_cxx_flags
----------------------------

.. dune:stanza:: use_standard_c_and_cxx_flags

   .. versionadded:: 2.8

   Control how flags coming from ``ocamlc -config`` are passed to the C
   compiler command line.

   Historically, they have been systematically prepended without a way to
   override them.

   If the following is passed, the mechanism is slightly altered:

   .. code:: dune

       (use_standard_c_and_cxx_flags)

   In this mode, Dune will populate the ``:standard`` set of C flags with the
   content of ``ocamlc_cflags`` and  ``ocamlc_cppflags``. These flags can be
   completed or overridden using the :doc:`/reference/ordered-set-language`.

   This is the default in the language version 3.0.

.. _using:

using
-----

.. dune:stanza:: using
   :param: <plugin> <version>

   Enable a dune language extension.

   The language of configuration files read by Dune can be extended to support
   additional stanzas (e.g., ``menhir``, ``coq.theory``, ``mdx``).

   `<plugin>` is the name of the plugin that defines this stanza and
   `<version>` describes the configuration language's version. Note that this
   version has nothing to do with the version of the associated tool or
   library. In particular, adding a ``using`` stanza will not result in a build
   dependency in the generated ``.opam`` file. See :ref:`generate_opam_files
   <generate_opam_files>`.

   Example:

   .. code:: dune

      (using mdx 0.3)

version
-------

.. dune:stanza:: version
   :param: <version>

   Set the version of the project.

   Example:

   .. code:: dune

      (version 1.2.3)

.. _wrapped-executables:

wrapped_executables
-------------------

.. dune:stanza:: wrapped_executables
   :param: <bool>

   .. versionadded:: 1.11

   Control wrapping of modules in executables.

   Executables are made of compilation units whose names may collide with
   libraries' compilation units. To avoid this possibility, Dune prefixes
   these compilation unit names with ``Dune__exe__``. This is entirely
   transparent to users except when such executables are debugged. In which
   case, the mangled names will be visible in the debugger.

   - with ``(wrapped_executables false)``, the original names are used.
   - with ``(wrapped_executables true)``, the names are mangled.

   Starting in language version 2.0, the default value is ``true``.

.. _map-workspace-root:

map_workspace_root
-------------------

.. dune:stanza:: map_workspace_root
   :param: <bool>

   Control references to the file system locations where the project has been
   built.

   - with ``(map_workspace_root true)``, dune rewrites references to the
     workspace root to ``/workspace_root``. Note that when this mapping is
     enabled, the debug information produced by the bytecode compiler is
     incorrect, as the location information is lost.

   - with ``(map_workspace_root false)``, the references are not rewritten.

   The default is ``(map_workspace_root true)``.

   .. versionadded:: 3.0
        Initial version with the mapping always enabled.
   .. versionchanged:: 3.7
        Add a way to disable the mapping.

.. _warnings:

warnings
--------

.. dune:stanza:: warnings

   .. versionadded:: 3.11

   Configure Dune warnings for the project.

   .. dune:field:: <name>
      :param: <enabled | disabled>

      Enable or disable the warning <name> for the current project.
