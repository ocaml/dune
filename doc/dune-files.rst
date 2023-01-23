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

   (lang dune 3.7)

Additionally, they can contains the following stanzas.

accept_alternative_dune_file_name
---------------------------------

Since Dune 3.0, it's possible to use the alternative filename ``dune-file``
instead of ``dune`` to specify the build. This may be useful to avoid problems
with ``dune`` files that have the executable permission in a directory in the
``PATH``, which can unwittingly happen in Windows.

The feature must be enabled explicitly by adding the following field to
``dune-project``:

.. code:: scheme

   (accept_alternative_dune_file_name)

Note that ``dune`` continues to be accepted even after enabling this option, but
if a file named ``dune-file`` is found in a directory, it will take precedence
over ``dune``.

cram
----

Enable or disable Cram-style tests for the project. See :ref:`cram-tests` for
details.

.. code:: scheme

   (cram <status>)

Where status is either ``enable`` or ``disable``.

.. _dialect:

dialect
-------

A dialect is an alternative frontend to OCaml (such as ReasonML). It's described
by a pair of file extensions, one corresponding to interfaces and one to
implementations.

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

For interfaces and implementations, ``(extension <string>)`` specifies the file
extension used for this dialect. The extension string must not contain any dots
and be unique in a given project (so that a given extension can be mapped back
to a corresponding dialect).

``<optional fields>`` are:

- Run ``(preprocess <action>)`` to produce a valid OCaml abstract syntax tree.
  It's expected to read the file given in the variable named ``input-file`` and
  output a *binary* abstract syntax tree on its standard output. See
  :ref:`preprocessing-actions` for more information.

  If the field isn't present, it's assumed that the corresponding source code is
  already valid OCaml code and can be passed to the OCaml compiler as-is.


- Run ``(format <action>)`` to format source code for this dialect. The action
  is expected to read the file given in the variable named ``input-file`` and
  output the formatted source code on its standard output. For more information.
  See :ref:`formatting-main` for more information.

  If the field is not present, then ``(preprocess <action>)`` is also not
  present (so that the dialect consists of valid OCaml code). In that case, the
  dialect will be formatted as any other OCaml code by default. Otherwise no
  special formatting will be done.

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

This option is enabled by default starting with Dune lang 3.0, so empty
interface files are no longer needed.

expand_aliases_in_sandbox
-------------------------

When a sandboxed action depends on an alias, copy the expansion of the alias
inside the sandbox. For instance, in the following example:

.. code:: scheme

    (alias
     (name foo)
     (deps ../x))

    (cram
     (deps (alias foo)))

File `x` will be visible inside the Cram test if and only if this option is
enabled. This option is a better default in general; however, it currently
causes Cram tests to run noticeably slower. So it is disabled by default until
the performance issue with Cram test is fixed.

.. _explicit-js-mode:

explicit_js_mode
----------------

Traditionally, JavaScript targets were defined for every bytecode executable.
This wasn't very precise and didn't interact well with the ``@all`` alias.

You can opt out of this behavior by using:

.. code:: scheme

    (explicit_js_mode)

When this mode is enabled, an explicit ``js`` mode needs to be added to the
``(modes ...)`` field of executables in order to trigger the JavaScript
compilation. Explicit JS targets declared like this will be attached to the
``@all`` alias.

Starting with Dune 2.0, this behavior is the default, and there is no way to
disable it.

.. _formatting:

formatting
----------

Starting in Dune 2.0, :ref:`formatting-main` is automatically enabled. This can
be controlled by using

.. code:: scheme

    (formatting <setting>)

where ``<setting>`` is one of:

- ``disabled``, meaning that automatic formatting is disabled

- ``(enabled_for <languages>)`` can be used to restrict the languages that are
  considered for formatting.

.. _generate_opam_files:

generate_opam_files
-------------------

Dune is able to use metadata specified in the ``dune-project`` file to generate
``.opam`` files (see :ref:`opam-generation`). To enable this integration, add
the following field to the ``dune-project`` file:

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

Some or all of these fields may be overridden for each package of the project,
see :ref:`package`.

.. _implicit_transitive_deps:

implicit_transitive_deps
------------------------

By default, Dune allows transitive dependencies of dependencies used when
compiling OCaml; however, this setting can be controlled per project:

.. code:: scheme

    (implicit_transitive_deps <bool>)

When set to ``false``, all dependencies directly used by a library or an
executable must be added in the ``libraries`` field. We recommend users
experiment with this mode and report any problems.

Note that you must use ``threads.posix`` instead of ``threads`` when using this
mode. This isn't an important limitation, as ``threads.vm`` is deprecated
anyway.

In some situations, it can be desirable to selectively preserve the behavior of
transitive dependencies' availability a library's users. For example, if we
define a library ``foo_more`` that extends ``foo``, we might want ``foo_more``
users to immediately have ``foo`` available as well. To do this, we must define
the dependency on ``foo`` as re-exported:

.. code:: scheme

   (library
    (name foo_more)
    (libraries (re_export foo)))

name
----

Sets the name of the project. It's used by :ref:`dune subst <dune-subst>` and
error messages.

.. code:: scheme

    (name <name>)

.. _package:

package
-------

Package specific information is specified in the ``(package <package-fields>)``
stanza. It contains the following fields:

- ``(name <string>)`` is the name of the package. This must be specified.

- ``(synopsis <string>)`` is a short package description.

- ``(description <string>)`` is a longer package description.

- ``(depends <dep-specification>)`` are package dependencies.

- ``(conflicts <dep-specification)`` are package conflicts.

- ``(depopts <dep-specification)`` are optional package dependencies.

- ``(tags <tags>)`` are the list of tags for the package.

- ``(deprecated_package_names <name list>)`` is a list of names that can be used
  with the :ref:`deprecated-library-name` stanza to migrate legacy libraries
  from other build systems that don't follow Dune's convention of prefixing the
  library's public name with the package name.

- ``(license <name>)``, ``(authors <authors>)``, ``(maintainers
  <maintainers>)``, ``(source <source>)``, ``(bug_reports <url>)``, ``(homepage
  <url>)``, and ``(documentation <url>)`` are the same (and take precedence
  over) the corresponding global fields. These fields have been available since
  Dune 2.0.

- ``(sites (<section> <name>) ...)`` define a site named ``<name>`` in the
  section ``<section>``.

Adding libraries to different packages is done via the ``public_name`` field.
See :ref:`library` section for details.

The list of dependencies :token:`dep_specification` is modelled after opam's own
language. The syntax is a list of the following elements:

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
ones listed in :token:`filter`. This also applies to version numbers. For example, to
generate ``depends: [ pkg { = version } ]``, use ``(depends (pkg (=
:version)))``.

Note that the use of a ``using`` stanza (see :ref:`using <using>`) doesn't
automatically add the associated library or tool as a dependency. They have to
be added explicitly.

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

.. _using:

using
-----

The language of configuration files read by Dune can be extended to support
additional stanzas (e.g., ``menhir``, ``coq.theory``, ``mdx``). This is done by
adding a line in the ``dune-project`` file, such as:

.. code:: scheme

    (using <plugin> <version>)

Here, ``<plugin>`` is the name of the plugin that defines this stanza and
``<version>`` describes the configuration language's version. Note that this
version has nothing to do with the version of the associated tool or library. In
particular, adding a ``using`` stanza will not result in a build dependency in
the generated ``.opam`` file. See :ref:`generate_opam_files
<generate_opam_files>`.

version
-------

Sets the version of the project:

.. code:: scheme

    (version <version>)

.. _wrapped-executables:

wrapped_executables
-------------------

Executables are made of compilation units whose names may collide with
libraries' compilation units. To avoid this possibility, Dune prefixes these
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

.. code:: scheme

    (lang dune 3.7)
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

.. code:: scheme

    (context (opam (switch <opam-switch-name>)
                   <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the subdirectory's name for ``_build``, where this
   build's context artifacts will be stored.

-  ``(root <opam-root>)`` is the opam root. By default, it will take the opam
   root defined by the environment in which ``dune`` is run, which is usually
   ``~/.opam``.

- ``(merlin)`` instructs Dune to use this build context for Merlin.

- ``(profile <profile>)`` sets a different profile for a build context. This has
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
  the value is specified using the :ref:`ordered-set-language`. Relative paths
  are interpreted with respect to the workspace root. See :ref:`finding-root`.

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

.. code:: scheme

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

.. code:: scheme

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

.. code:: scheme

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

.. code:: scheme

    (cache-check-probability <number>)

where ``<number>`` is a floating-point number between 0 and 1 (inclusive). 0
means never to check for reproducibility, and 1 means to always perform the
check.

.. _cache_storage_mode:

cache-storage-mode
------------------

Specify the mechanism used by the Dune cache for storage.

.. code:: scheme

    (cache-storage-mode <setting>)

where ``<setting>`` is one of:

- ``auto`` lets Dune decide the best mechanism to use.

- ``hardlink`` uses hard links for entries in the cache. If the cache is stored
  in a different partition than the one where the build is taking place, then
  this mode will not work and ``copy`` should be used instead.

- ``copy`` copies entries to the cache. This is less efficient than using hard
  links.

.. _concurrency:

concurrency
-----------

Maximum number of concurrent jobs Dune is allowed to have.

.. code:: scheme

    (concurrency <setting>)

where ``<setting>`` is one of:

- ``auto``, auto-detect maximum number of cores. This is the default value.

- ``<number>``, a positive integer specifying the maximum number of jobs Dune
  may use simultaneously.

.. _display:

display
-------

Specify the amount of Dune’s verbosity.

.. code:: scheme

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

.. code:: scheme

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

.. code:: scheme

    (terminal-persistence <setting>)

where ``<setting>`` is one of:

- ``preserve`` does not clear the terminal screen beteween rebuilds.

- ``clear-on-rebuild`` clears the terminal screen between rebuilds.

- ``clear-on-rebuild-and-flush-history`` clears the terminal between rebuilds, and
  it also deletes everything in the scrollback buffer.
