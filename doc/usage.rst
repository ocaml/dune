**********************
Command-Line Interface
**********************

This section describes using ``dune`` from the shell.

.. _initializing_components:

Initializing Components
=======================

NOTE: The ``dune init`` command is still under development and subject to
change.

Dune's ``init`` subcommand provides limited support for generating Dune file
stanzas and folder structures to define components. The ``dune init`` command can be used to
quickly add new projects, libraries, tests, and executables without having to
manually create Dune files in a text editor, or it can be composed to programmatically generate
parts of a multi-component project.

Initializing a Project
----------------------

You can run the following command to initialize a new Dune project that uses the ``base`` and ``cmdliner``
libraries and supports inline tests:

.. code:: bash

   $ dune init proj myproj --libs base,cmdliner --inline-tests --ppx ppx_inline_test

This creates a new directory called ``myproj``, including subdirectories and
``dune`` files for library, executable, and test components. Each component's
``dune`` file will also include the declarations required for the given
dependencies.

This is the quickest way to get a basic ``dune`` project up and building.

Initializing an Executable
-----------------------------

To add a new executable to a ``dune`` file in the current directory
(creating the file if necessary), run

.. code:: bash

    $ dune init exe myexe --libs base,containers,notty --ppx ppx_deriving

This will add the following stanza to the ``dune`` file:

.. code:: scheme

    (executable
     (name main)
     (libraries base containers notty)
     (preprocess
      (pps ppx_deriving)))

Initializing a Library
----------------------

Run the following command to create a new directory ``src``, initialized as a library:

.. code:: bash

    $ dune init lib mylib src --libs core --inline-tests --public

This will ensure the file ``./src/dune`` contains the below stanza (creating
the file and directory, if necessary):

.. code:: scheme

    (library
     (public_name mylib)
     (inline_tests)
     (name mylib)
     (libraries core)
     (preprocess
      (pps ppx_inline_tests)))

Consult the manual page using the ``dune init --help`` command for more details.

.. _finding-root:

Finding the Root
================

The root of the current workspace is determined by looking up a
``dune-workspace`` or ``dune-project`` file in the current directory and its
parent directories. Dune requires at least one of these two files to operate.

If it isn't in the current directory, Dune prints out the root when starting:

.. code:: bash

    $ dune runtest
    Entering directory '/home/jdimino/code/dune'
    ...

This message can be suppressed with the ``--no-print-directory``
command line option (as in GNU make).

More precisely, Dune will choose the outermost ancestor directory containing a
``dune-workspace`` file, which is used to mark the root of the current workspace.
If no ``dune-workspace`` file is present, the same strategy applies with
``dune-project`` files.

In case of a mix of `dune-workspace` and `dune-project` files, workspace files
take precedence over project files in the sense that if a ``dune-workspace``
file is found, only parent ``dune-workspace`` files will be considered when
looking for the root; however, if a `dune-project` file is found both parent
``dune-workspace`` and ``dune-project`` files will be considered.

A ``dune-workspace`` file is also a configuration file. Dune will read
it unless the ``--workspace`` command line option is used.  See the
section :ref:`dune-workspace` for the syntax of this file. The scope
of ``dune-project`` files is wider than the scope ``dune-workspace``
files. For instance, a ``dune-project`` file may specify the name of
the project which is a universal property of the project, while a
``dune-workspace`` file may specify an opam switch name which is valid
only on a given machine. For this reason, it is common and recommended
to commit ``dune-project`` files in repositories, while it is less
common to commit ``dune-workspace`` files.


Current Directory
-----------------

If the previous rule doesn't apply, i.e., no ancestor directory has a
file named ``dune-workspace``, then the current directory will be used
as root.

Forcing the Root (for Scripts)
------------------------------

You can pass the ``--root`` option to ``dune`` to select the root
explicitly. This option is intended for scripts to disable the automatic lookup.

Note that when using the ``--root`` option, targets given on the command line
will be interpreted relative to the given root, not relative to the current
directory, as this is normally the case.

Interpretation of Targets
=========================

This section describes how Dune interprets the targets provided on
the command line. When no targets are specified, Dune builds the
``default`` alias, see :ref:`default-alias` for more details.

Resolution
----------

All targets that Dune knows how to build live in the ``_build`` directory.
Although, some are sometimes copied to the source tree for the need of external
tools. These includes ``<package>.install`` files when either ``-p`` or
``--promote-install-files`` is passed on the command line.

As a result, if you want to ask Dune to produce a particular ``.exe``
file you would have to type:

.. code:: bash

    $ dune build _build/default/bin/prog.exe

However, for convenience, when a target on the command line doesn't
start with ``_build``, Dune expands it to the
corresponding target in all the build contexts that Dune knows how to
build. When using ``--verbose``, it prints out the actual set of
targets upon starting:

.. code:: bash

    $ dune build bin/prog.exe --verbose
    ...
    Actual targets:
    - _build/default/bin/prog.exe
    - _build/4.03.0/bin/prog.exe
    - _build/4.04.0/bin/prog.exe

Aliases
-------

Targets starting with a ``@`` are interpreted as aliases. For instance
``@src/runtest`` means the alias ``runtest`` in all descendants of
``src`` in all build contexts where it is defined. If you want to
refer to a target starting with a ``@``, simply write: ``./@foo``.

To build and run the tests for a particular build context, use
``@_build/default/runtest`` instead.

For instance:

-  ``dune build @_build/foo/runtest`` only runs the tests for
   the ``foo`` build context
-  ``dune build @runtest`` will run the tests for all build contexts

You can also build an alias non-recursively by using ``@@`` instead of
``@``. For instance, to run tests only from the current directory, use:

.. code::

   dune build @@runtest

Please note: it's not currently possible to build a target directly if that target
lives in a directory that starts with the ``@`` character. In the rare cases
where you need to do that, you can declare an alias like so:

.. code:: scheme

    (alias
     (name foo)
     (deps @foo/some.exe))

``@foo/some.exe`` can then be built with:

.. code::

   dune build @foo


.. _default-alias:

Default Alias
-------------

When no targets are given to ``dune build``, it builds the special
``default`` alias. Effectively ``dune build`` is equivalent to:

.. code::

   dune build @@default

When a directory doesn't explicitly define what the ``default`` alias
means via an :ref:`alias-stanza` stanza, the following implicit
definition is assumed:

.. code::

   (alias
    (name default)
    (deps (alias_rec all)))

Which means that by default ``dune build`` will build everything that
is installable.

When using a directory as a target, it will be interpreted as building the
default target in the directory. The directory must exist in the source tree.

.. code::

   dune build dir

Is equivalent to:

.. code::

   dune build @@dir/default

.. _builtin-aliases:

Built-in Aliases
----------------

There are a few aliases that Dune automatically creates for the user:

* ``default`` includes all the targets that Dune will build if a
  target isn't specified, i.e., ``$ dune build``. By default, this is set to the
  ``all`` alias. Note that for Dune 1.x, this was initially set to the ``install`` alias.

* ``runtest`` runs all the tests, building them if
  necessary.

* ``install`` builds all public artifacts that will be installed.

* ``doc`` builds documentation for public libraries.

* ``doc-private`` builds documentation for all libraries, both public & private.

* ``lint`` runs linting tools.

* ``all`` builds all available targets in a directory and also builds installable artifacts
  defined in that directory.

* ``check`` builds the minimal set of targets required for
  tooling support. Essentially, this is ``.cmi``, ``.cmt``, and ``.cmti`` files and
  Merlin configurations.

Variables for Artifacts
-----------------------

It's possible to build specific artifacts by using the corresponding variable
on the command line. For example:

.. code::

    dune build '%{cmi:foo}'

See :ref:`variables-for-artifacts` for more information.


Finding External Libraries
==========================

When a library isn't available in the workspace, Dune will search for it
in the installed world and expect it to be already compiled.

It looks up external libraries using a specific list of search paths,
and each build context has a specific list of search paths.

When running inside an opam environment, Dune will look for installed
libraries in ``$OPAM_SWITCH_PREFIX/lib``. This includes both opam
build context configured via the ``dune-workspace`` file and the
default build context when the variable ``$OPAM_SWITCH_PREFIX`` is
set.

Otherwise, Dune takes the directory where ``ocamlc`` was found and
appends `../lib`` to it. For instance, if ``ocamlc`` is found in
``/usr/bin``, Dune looks for installed libraries in ``/usr/lib``.

In addition to the two above rules, Dune always inspects the
``OCAMLPATH`` environment variable and uses the paths defined in this
variable. ``OCAMLPATH`` always has precedence and can have different
values in different build contexts. For instance, you can set it
manually in a specific build context via the ``dune-workspace`` file.

.. _running-tests:

Running Tests
=============

There are two ways to run tests:

-  ``dune build @runtest``
-  ``dune test`` (or the more explicit ``dune runtest``)

The two commands are equivalent, and they will run all the tests defined in the
current directory and its children directories recursively. You can also run the tests in a
specific sub-directory and its children by using:

-  ``dune build @foo/bar/runtest``
-  ``dune test foo/bar`` (or ``dune runtest foo/bar``)

Watch Mode
==========

The ``dune build`` and ``dune runtest`` commands support a ``-w`` (or
``--watch``) flag. When it's passed, Dune will perform the action as usual and
then wait for file changes and rebuild (or rerun the tests). This feature
requires ``inotifywait`` or ``fswatch`` to be installed.

Launching the Toplevel (REPL)
=============================

Dune supports launching a `utop <https://github.com/diml/utop>`__ instance
with locally defined libraries loaded.

.. code:: bash

   $ dune utop <dir> -- <args>

Where ``<dir>`` is a directory under which Dune searches (recursively) for
all libraries that will be loaded. ``<args>`` will be passed as arguments to the
``utop`` command itself. For example, ``dune utop lib -- -implicit-bindings`` will
start ``utop``, with the libraries defined in ``lib`` and implicit bindings for
toplevel expressions.

Dune also supports loading individual modules unsealed by their signatures into
the toplevel. This is accomplished by launching a toplevel and then asking dune
to return the toplevel directives needed to evaluate the module:

.. code:: bash

   $ utop
   # use_output "dune top-module path/to/module.ml";;

Requirements & Limitations
--------------------------

* Utop version >= 2.0 is required for this to work.
* This subcommand only supports loading libraries. Executables aren't supported.
* Libraries that are dependencies of utop itself cannot be loaded. For example
  `Camomile <https://github.com/yoriyuki/Camomile>`__.
* Loading libraries that are defined in different directories into one ``utop``
  instance isn't possible.

Restricting the Set of Packages
===============================

Restrict the set of packages from your workspace that Dune can see with
the ``--only-packages`` option:

.. code:: bash

    $ dune build --only-packages pkg1,pkg2,... @install

This option acts as if you went through all the Dune files and
commented out the stanzas referring to a package that isn't in the list
given to ``dune``.

Distributing Projects
=====================

Dune provides support for building and installing your project; however, it
doesn't provide helpers for distributing it. It's recommended to use
`dune-release <https://github.com/samoht/dune-release>`__ for this purpose.

The common defaults are that your projects include the following files:

- ``README.md``
- ``CHANGES.md``
- ``LICENSE.md``

If your project contains several packages, all the package names
must be prefixed by the shortest one.

.. _dune-subst:

dune subst
==========

One of the features ``dune-release`` provides is watermarking; it replaces
various strings of the form ``%%ID%%`` in all your project files
before creating a release tarball or when the opam user pins the package.

This is especially interesting for the ``VERSION`` watermark, which gets
replaced by the version obtained from the Version-Control System (VCS). For instance, if you're using
Git, ``dune-release`` invokes this command to find out the version:

.. code:: bash

    $ git describe --always --dirty --abbrev=7
    1.0+beta9-79-g29e9b37

If no VCS is detected, ``dune subst`` will do nothing.

Projects using Dune usually only need ``dune-release`` for creating and
publishing releases. However, they may still substitute the
watermarks when the user pins the package. To help with this,
Dune provides the ``subst`` sub-command.

``dune subst`` performs the same substitution that ``dune-release`` does
with the default configuration, i.e., calling ``dune subst`` at the
root of your project will rewrite all your project files.

More precisely, it replaces the following watermarks in the source files:

- ``NAME``, the name of the project
- ``VERSION``, output of ``git describe --always --dirty --abbrev=7``
- ``VERSION_NUM``, same as ``VERSION`` but with a potential leading
  ``v`` or ``V`` dropped
- ``VCS_COMMIT_ID``, commit hash from the vcs
- ``PKG_MAINTAINER``, contents of the ``maintainer`` field from the
  opam file
- ``PKG_AUTHORS``, contents of the ``authors`` field from the opam file
- ``PKG_HOMEPAGE``, contents of the ``homepage`` field from the opam file
- ``PKG_ISSUES``, contents of the ``issues`` field from the opam file
- ``PKG_DOC``, contents of the ``doc`` field from the opam file
- ``PKG_LICENSE``, contents of the ``license`` field from the opam file
- ``PKG_REPO``, contents of the ``repo`` field from the opam file

The project name is obtained by reading the ``dune-project``
file in the directory where ``dune subst`` is called. The
``dune-project`` file must exist and contain a valid ``(name ...)``
field.

Note that ``dune subst`` is meant to be called from the opam file and
behaves a bit different to other Dune commands. In
particular it doesn't try to detect the root of the workspace and must
be called from the root of the project.

Custom Build Directory
======================

By default Dune places all build artifacts in the ``_build`` directory relative
to the user's workspace. However, one can customize this directory by using the
``--build-dir`` flag or the ``DUNE_BUILD_DIR`` environment variable.

.. code:: bash

   $ dune build --build-dir _build-foo

   # this is equivalent to:
   $ DUNE_BUILD_DIR=_build-foo dune build

   # Absolute paths are also allowed
   $ dune build --build-dir /tmp/build foo.exe

Installing a Package
====================

Via opam
--------

When releasing a package using Dune in opam, there's nothing special
to do.  Dune generates a file called ``<package-name>.install`` at the
root of the project.  This contains a list of files to install, and
opam reads it in order to perform the installation.

Manually
--------

When not using opam, or when you want to manually install a package,
you can ask Dune to perform the installation via the ``install``
command:

::

    $ dune install [PACKAGE]...

This command takes a list of package names to install.  If no packages
are specified, Dune will install all available packages in the
workspace.  When several build contexts are specified via a
:ref:`dune-workspace` file, Dune performs the installation in all the
build contexts.

Destination Directory
---------------------

For a given build context, the installation directories are determined with a
single scheme for all installation sections. Taking the ``lib`` installation
section as an example, the priorities of this scheme are as follows:

#. if an explicit ``--lib <path>`` argument is passed, use this path
#. if an explicit ``--prefix <path>`` argument is passed, use ``<path>/lib``
#. if ``--lib <path>`` argument is passed before during dune compilation to
   ``./configure``, use this paths
#. if ``OPAM_SWITCH_PREFIX`` is present in the environment use ``$OPAM_SWITCH_PREFIX/lib``
#. otherwise, fail

Relocation Mode
---------------

The installation can be done in specific mode (``--relocation``) for creating a
directory that can be moved. In that case, the installed executables will
look up the package sites (cf :ref:`sites`) relative to its location.
The `--prefix` directory should be used to specify the destination.


If you're using plugins that depend on installed libraries and aren't
executable dependencies, like libraries that need to be loaded at
runtime, you must copy the libraries manually to the destination directory.

Querying Merlin Configuration
=============================

Since Version 2.8, Dune no longer promotes ``.merlin`` files to the source
directories. Instead, Dune stores these configurations in the `_build`
folder, and Merlin communicates directly with Dune to obtain its configuration
via the `ocaml-merlin` subcommand. The Merlin configuration is now stanza-specific,
allowing finer control. The following commands aren't needed for
normal Dune and Merlin use, but they can provide insightful information when
debugging or configuring non-standard projects.

Printing the Configuration
--------------------------

It's possible to manually query the generated configuration for debugging
purposes:

::

    $ dune ocaml-merlin --dump-config

This command prints the distinct configuration of each module present in the
current directory. This directory must be in a Dune workspace and the project
must be already built. The configuration will be encoded as s-expressions, which
are used to communicate with Merlin.

Printing an Approximated ``.merlin``
------------------------------------

It's also possible to print the current folder's configuration in the
Merlin configuration syntax by running the following command:

::

    $ dune ocaml dump-dot-merlin > .merlin

In that case, Dune prints only one configuration: the result of the configuration's
coarse merge in the current folder's various modules.
This folder must be in a Dune workspace, and the project must be already
built. Preprocessing directives and other flags will be commented out and must
be un-commented afterward. This feature doesn't aim at writing exact or correct
``.merlin`` files. Its sole purpose is to lessen the burden of writing the
configuration from scratch.

.. _merlin-filenames:

Non-Standard Filenames
----------------------

Merlin configuration loading is based on filenames, so if you have
files that are preprocessed by custom rules before they are built, they should
respect the following naming convention: the unprocessed file should start with
the name of the resulting processed file followed by a dot. The rest
does not matter. Dune uses only the name before the first dot to
match with available configurations.

For example, if you use the ``cppo`` preprocessor to generate the file
``real_module_name.ml``, then the source file could be named
``real_module_name.cppo.ml``.

Running a Coq Toplevel
======================

See :ref:`running-coq-top`.
