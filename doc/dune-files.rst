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

.. code:: dune

   (lang dune 3.8)

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

      .. dune:field:: extension
         :param: <string>

         Specify the file extension used for this dialect.

         The extension string must not contain any dots and be unique in a given
         project (so that a given extension can be mapped back to a
         corresponding dialect).

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

         .. seealso:: :ref:`formatting-main`

   .. dune:field:: interface

      Details related to the interface files (corresponding to `*.mli`).

      This field supports the same sub-fields as ``implementation``.

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
         (enabled_for <languages>)

     The list of `<languages>` can be either ``dune`` (formatting of dune
     files) or a :term:`dialect` name.

   .. seealso:: :ref:`formatting-main`

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

   .. seealso:: :ref:`opam-generation`

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

   Control how flags coming grom ``ocamlc -config`` are passed to the C
   compiler command line.

   Historically, they have been systematically prepended without a way to
   override them.

   If the following is passed, the mechanism is slightly altered:

   .. code:: dune

       (use_standard_c_and_cxx_flags)

   In this mode, Dune will populate the ``:standard`` set of C flags with the
   content of ``ocamlc_cflags`` and  ``ocamlc_cppflags``. These flags can be
   completed or overridden using the :doc:`reference/ordered-set-language`.

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

   .. versionadded:: 3.7

   Control references to the file system locations where the project has been
   built.

   - with ``(map_workspace_root true)``, dune rewrites references to the
     workspace root to ``/workspace_root``. Note that when this mapping is
     enabled, the debug information produced by the bytecode compiler is
     incorrect, as the location information is lost.
   - with ``(map_workspace_root false)``, the references are not rewritten.

   Starting from language version 3.0, the default is ``true``.

.. _dune-files:

dune
====

``dune`` files are the main part of Dune. They are used to describe libraries,
executables, tests, and everything Dune needs to know about.

The syntax of ``dune`` files is described in
:doc:`reference/lexical-conventions`.

``dune`` files are composed of stanzas, as shown below:

.. code:: dune

    (library
     (name mylib)
     (libraries base lwt))

    (rule
     (target foo.ml)
     (deps   generator/gen.exe)
     (action (run %{deps} -o %{target})))

The following sections describe the available stanzas and their meanings.

.. include:: stanzas/alias.rst
.. include:: stanzas/cinaps.rst
.. include:: stanzas/copy_files.rst
.. include:: stanzas/coq_theory.rst
.. include:: stanzas/data_only_dirs.rst
.. include:: stanzas/deprecated_library_name.rst
.. include:: stanzas/dirs.rst
.. include:: stanzas/documentation.rst
.. include:: stanzas/env.rst
.. include:: stanzas/executable.rst
.. include:: stanzas/external_variant.rst
.. include:: stanzas/foreign_library.rst
.. include:: stanzas/generate_sites_module.rst
.. include:: stanzas/ignored_subdirs.rst
.. include:: stanzas/include.rst
.. include:: stanzas/include_subdirs.rst
.. include:: stanzas/install.rst
.. include:: stanzas/jbuild_version.rst
.. include:: stanzas/library.rst
.. include:: stanzas/mdx.rst
.. include:: stanzas/menhir.rst
.. include:: stanzas/ocamllex.rst
.. include:: stanzas/ocamlyacc.rst
.. include:: stanzas/plugin.rst
.. include:: stanzas/rule.rst
.. include:: stanzas/subdir.rst
.. include:: stanzas/test.rst
.. include:: stanzas/toplevel.rst
.. include:: stanzas/vendored_dirs.rst

.. _dune-workspace:

dune-workspace
==============

By default, a workspace has only one build context named ``default`` which
corresponds to the environment, in which ``dune`` is run. You can define more
contexts by writing a ``dune-workspace`` file.

You can point Dune to an explicit ``dune-workspace`` file with the
``--workspace`` option. For instance, it's good practice to write a
``dune-workspace.dev`` in your project with all the OCaml versions your projects
support, so developers can test that the code builds with all OCaml versions by
simply running:

.. code:: bash

    $ dune build --workspace dune-workspace.dev @all @runtest

The ``dune-workspace`` file uses the S-expression syntax. This is what a typical
``dune-workspace`` file looks like:

.. code:: dune

    (lang dune 3.8)
    (context (opam (switch 4.07.1)))
    (context (opam (switch 4.08.1)))
    (context (opam (switch 4.11.1)))

The rest of this section describe the stanzas available.

Note that an empty ``dune-workspace`` file is interpreted the same as one
containing exactly:

.. code:: dune

    (lang dune 3.2)
    (context default)

This allows you to use an empty ``dune-workspace`` file to mark the root of your
project.

env
---

The ``env`` stanza can be used to set the base environment for all contexts in
this workspace. This environment has the lowest precedence of all other ``env``
stanzas. The syntax for this stanza is the same as Dune's :ref:`dune-env`
stanza.

``config`` stanzas
------------------

Starting in Dune 3.0, any of the stanzas from the :ref:`config` file can be used
in the ``dune-workspace`` file. In this case, the configuration stanza will only
affect the current workspace.

context
-------

The ``(context ...)`` stanza declares a build context. The argument can be
either ``default`` or ``(default)`` for the default build context, or it can be
the description of an opam switch, as follows:

.. code:: dune

    (context (opam (switch <opam-switch-name>)
                   <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the subdirectory's name for ``_build``, where this
   build's context artifacts will be stored.

-  ``(root <opam-root>)`` is the opam root. By default, it will take the opam
   root defined by the environment in which ``dune`` is run, which is usually
   ``~/.opam``.

- ``(merlin)`` instructs Dune to use this build context for Merlin.

- ``(profile <profile>)`` sets a different profile for a :term:`build context`. This has
  precedence over the command-line option ``--profile``.

- ``(env <env>)`` sets the environment for a particular context. This is of
  higher precedence than the root ``env`` stanza in the workspace file. This
  field has the same options as the :ref:`dune-env` stanza.

- ``(toolchain <findlib_toolchain>)`` sets a ``findlib`` toolchain for the
  context.

- ``(host <host_context>)`` chooses a different context to build binaries that
  are meant to be executed on the host machine, such as preprocessors.

- ``(paths (<var1> <val1>) .. (<varN> <valN>))`` allows you to set the value of
  any ``PATH``-like variables in this context. If ``PATH`` itself is modified in
  this way, its value will be used to resolve workspace binaries, including
  finding the compiler and related tools. These variables will also be passed as
  part of the environment to any program launched by Dune. For each variable,
  the value is specified using the :doc:`reference/ordered-set-language`.
  Relative paths are interpreted with respect to the workspace root. See
  :ref:`finding-root`.

- ``(fdo <target_exe>)`` builds this context with feedback-direct optimizations.
  It requires `OCamlFDO <https://github.com/gretay-js/ocamlfdo>`__.
  ``<target_exe>`` is a path-interpreted relative to the workspace root (see
  :ref:`finding-root`). ``<target_exe>`` specifies which executable to optimize.
  Users should define a different context for each target executable built with
  FDO. The context name is derived automatically from the default name and
  ``<target-exe>``, unless explicitly specified using the ``(name ...)`` field.
  For example, if ``<target_exe>`` is *src/foo.exe* in a default context, then
  the name of the context is *default-fdo-foo* and the filename that contains
  execution counters is *src/fdo.exe.fdo-profile*.  This feature is
  **experimental** and no backwards compatibility is implied.

- By default, Dune builds and installs dynamically-linked foreign archives
  (usually named ``dll*.so``). It's possible to disable this by setting by
  including ``(disable_dynamically_linked_foreign_archives true)`` in the
  workspace file, so bytecode executables will be built with all foreign
  archives statically linked into the runtime system.


Both ``(default ...)`` and ``(opam ...)`` accept a ``targets`` field in order to
setup cross compilation. See :ref:`cross-compilation` for more information.

Merlin reads compilation artifacts, and it can only read the compilation
artifacts of a single context. Usually, you should use the artifacts from the
``default`` context, and if you have the ``(context default)`` stanza in your
``dune-workspace`` file, that is the one Dune will use.

For rare cases where this is not what you want, you can force Dune to use a
different build contexts for Merlin by adding the field ``(merlin)`` to this
context.

profile
-------

The build profile can be selected in the ``dune-workspace`` file by write a
``(profile ...)`` stanza. For instance:

.. code:: dune

    (profile release)

Note that the command line option ``--profile`` has precedence over this stanza.

.. _config:

config
======

This file is used to set the global configuration of Dune which is applicable
across projects and workspaces.

The configuration file is normally ``~/.config/dune/config`` on Unix systems and
``%LOCALAPPDATA%/dune/config`` on Windows. However, for most Dune commands it is
possible to specify an alternative configuration file with the ``--config-file``
option. Command-line flags take precedence over the contents of the ``config``
file.  If ``--no-config`` or ``-p`` is passed, Dune will not read this file.

The ``config`` file can contain the following stanzas:

.. _action_stderr_on_success:

action_stderr_on_success
------------------------

Same as :ref:`action_stdout_on_success`, but applies to standard error instead
of standard output.

.. _action_stdout_on_success:

action_stdout_on_success
------------------------

Specifies how Dune should handle the standard output of actions when they succeed.
This can be used to reduce the noise of large builds.

.. code:: dune

    (action_stdout_on_success <setting>)

where ``<setting>`` is one of:

- ``print`` prints the output on the terminal. This is the default.

- ``swallow`` ignores the output and does not print it on the terminal.

- ``must-be-empty`` enforces that the output should be empty. If it is not, Dune will fail.

.. _cache:

cache
-----

Specifies whether Dune is allowed to store and fetch build targets from the Dune
cache.

.. code:: dune

    (cache <setting>)

where ``<setting>`` is one of:

- ``enabled`` enables Dune cache.

- ``disabled`` disables Dune cache.

.. _cache_check_probability:

cache-check-probability
-----------------------

While the main purpose of Dune cache is to speed up build times, it can also be
used to check build reproducibility. It is possible to enable a probabilistic
check, in which Dune will re-execute randomly chosen build rules and compare
their results with those stored in the cache. If the results differ, the rule is
not reproducible, and Dune will print out a corresponding warning.

.. code:: dune

    (cache-check-probability <number>)

where ``<number>`` is a floating-point number between 0 and 1 (inclusive). 0
means never to check for reproducibility, and 1 means to always perform the
check.

.. _cache_storage_mode:

cache-storage-mode
------------------

Specify the mechanism used by the Dune cache for storage.

.. code:: dune

    (cache-storage-mode <setting>)

where ``<setting>`` is one of:

- ``auto`` lets Dune decide the best mechanism to use.

- ``hardlink`` uses hard links for entries in the cache. If the cache is stored
  in a different partition than the one where the build is taking place, then
  this mode will not work and ``copy`` should be used instead.

- ``copy`` copies entries to the cache. This is less efficient than using hard
  links.

.. _jobs:

jobs
----

Maximum number of concurrent jobs Dune is allowed to have.

.. code:: dune

    (jobs <setting>)

where ``<setting>`` is one of:

- ``auto``, auto-detect maximum number of cores. This is the default value.

- ``<number>``, a positive integer specifying the maximum number of jobs Dune
  may use simultaneously.

.. _display:

display
-------

Specify the amount of Dune’s verbosity.

.. code:: dune

    (display <setting>)

where ``<setting>`` is one of:

- ``progress``, Dune shows and updates a status line as build goals are being
  completed. This is the default value.

- ``verbose`` prints the full command lines of programs being executed by Dune,
  with some colors to help differentiate programs.

- ``short`` prints a line for each program executed with the binary name on the
  left and the targets of the action on the right.

- ``quiet`` only display errors.

.. _sandboxing_preference:

sandboxing_preference
---------------------

The preferred sandboxing setting. Individual rules may specify different
preferences. Dune will try to utilize a setting satisfying both conditions.

.. code:: dune

    (sandboxing_preference <setting> <setting> ...)

where each ``<setting>`` can be one of:

- ``none`` disables sandboxing.

- ``hardlink`` uses hard links for sandboxing. This is the default under Linux.

- ``copy`` copies files for sandboxing. This is the default under Windows.

- ``symlink`` uses symbolic links for sandboxing.

.. _terminal-persistence:

terminal-persistence
--------------------

Specifies how Dune handles the terminal when a rebuild is triggered in watch mode.

.. code:: dune

    (terminal-persistence <setting>)

where ``<setting>`` is one of:

- ``preserve`` does not clear the terminal screen beteween rebuilds.

- ``clear-on-rebuild`` clears the terminal screen between rebuilds.

- ``clear-on-rebuild-and-flush-history`` clears the terminal between rebuilds, and
  it also deletes everything in the scrollback buffer.
