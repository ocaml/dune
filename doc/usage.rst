**********************
Command-line interface
**********************

This section describe usage of dune from the shell.

.. _initializing_components:

Initializing components
=======================

NOTE: The ``dune init`` command is still under development and subject to
change.

Dune's ``init`` subcommand provides limited support for generating dune file
stanzas and folder structures to define components. ``dune init`` can be used to
quickly add new projects, libraries, tests, or executables without having to
manually create dune files, or it can be composed to programmatically generate
parts of a multi-component project.

Initializing a project
----------------------

To initialize a new ``dune`` project that uses the ``base`` and ``cmdliner``
libraries and supports inline tests, you can run

.. code:: bash

   $ dune init proj myproj --libs base,cmdliner --inline-tests --ppx ppx_inline_test

This will create a new directory called ``myproj`` including sub directories and
``dune`` files for library, executable, and test components. Each component's
``dune`` file will also include the declarations required for the given
dependencies.

This is the quickest way to get a basic ``dune`` project up and building.

Initializing an executable
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

Initializing a library
----------------------

To create a new directory ``src``, initialized as a library, you can run:

.. code:: bash

    $ dune init lib mylib src --libs core --inline-tests --public

This will ensure the file ``./src/dune`` contains the following stanza (creating
the file and directory, if needed):

.. code:: scheme

    (library
     (public_name mylib)
     (inline_tests)
     (name mylib)
     (libraries core)
     (preprocess
      (pps ppx_inline_tests)))

Consult the manual page ``dune init --help`` for more details.

.. _finding-root:

Finding the root
================

The root of the current workspace is determined by looking up a
``dune-workspace`` or ``dune-project`` file in the current directory
and parent directories.

``dune`` prints out the root when starting if it is not the current
directory:

.. code:: bash

    $ dune runtest
    Entering directory '/home/jdimino/code/dune'
    ...

More precisely, it will choose the outermost ancestor directory containing a
``dune-workspace`` file as root. For instance if you are in
``/home/me/code/myproject/src``, then dune will look for all these files in
order:

-  ``/dune-workspace``
-  ``/home/dune-workspace``
-  ``/home/me/dune-workspace``
-  ``/home/me/code/dune-workspace``
-  ``/home/me/code/myproject/dune-workspace``
-  ``/home/me/code/myproject/src/dune-workspace``

The first entry to match in this list will determine the root. In
practice this means that if you nest your workspaces, dune will
always use the outermost one.

In addition to determining the root, ``dune`` will read this file
to setup the configuration of the workspace unless the ``--workspace``
command line option is used. See the section :ref:`dune-workspace`
for the syntax of this file.

The ``Entering directory`` message can be suppressed with the
``--no-print-directory`` command line option (as in GNU make).

Current directory
-----------------

If the previous rule doesn't apply, i.e. no ancestor directory has a
file named ``dune-workspace``, then the current directory will be used
as root.

Forcing the root (for scripts)
------------------------------

You can pass the ``--root`` option to ``dune`` to select the root
explicitly. This option is intended for scripts to disable the automatic lookup.

Note that when using the ``--root`` option, targets given on the command line
will be interpreted relative to the given root, not relative to the current
directory as this is normally the case.

Interpretation of targets
=========================

This section describes how ``dune`` interprets the targets given on
the command line. When no targets are specified, ``dune`` builds the
``default`` alias, see :ref:`default-alias` for more details.

Resolution
----------

All targets that dune knows how to build live in the ``_build``
directory.  Although, some are sometimes copied to the source tree for
the need of external tools. These includes:

- ``.merlin`` files
- ``<package>.install`` files (when either ``-p`` or
  ``--promote-install-files`` is passed on the command line)

As a result, if you want to ask ``dune`` to produce a particular ``.exe``
file you would have to type:

.. code:: bash

    $ dune build _build/default/bin/prog.exe

However, for convenience when a target on the command line doesn't
start with ``_build``, ``dune`` will expand it to the
corresponding target in all the build contexts where it knows how to
build it. When using ``--verbose``, It prints out the actual set of
targets when starting:

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
``@src/runtest`` means the alias ``runtest`` in all descendant of
``src`` in all build contexts where it is defined. If you want to
refer to a target starting with a ``@``, simply write: ``./@foo``.

To build and run the tests for a particular build context, use
``@_build/default/runtest`` instead.

So for instance:

-  ``dune build @_build/foo/runtest`` will run the tests only for
   the ``foo`` build context
-  ``dune build @runtest`` will run the tests for all build contexts

You can also build an alias non-recursively by using ``@@`` instead of
``@``. For instance to run tests only from the current directory:

.. code::

   dune build @@runtest

Note that it's currently not possible to build a target directly if that target
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

Default alias
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

There's a few aliases that dune automatically creates for the user

* ``default`` - this alias includes all the targets that dune will build if a
  target isn't specified, i.e. ``$ dune build``. By default, this is set to the
  ``all`` alias. Note that for dune 1.x, this was set to the ``install`` alias.

* ``runtest`` - this is the alias to run all the tests, building them if
  necessary.

* ``install`` - build all public artifacts - those that will be installed.

* ``doc`` - build documentation for public libraries.

* ``doc-private`` - build documentation for all libraries - public & private.

* ``lint`` - run linting tools.

* ``all`` - build all available targets in a directory and installable artifacts
  defined in that directory.

* ``check`` - This alias will build the minimal set of targets required for
  tooling support. Essentially, this is ``.cmi``, ``.cmt``, ``.cmti``, and
  ``.merlin`` files.

Variables for artifacts
-----------------------

It is possible to build specific artifacts by using the corresponding variable
on the command line, e.g.:

.. code::

    dune build '%{cmi:foo}'

See :ref:`variables-for-artifacts` for more information.


Finding external libraries
==========================

When a library is not available in the workspace, dune will look it
up in the installed world, and expect it to be already compiled.

It looks up external libraries using a specific list of search paths. A
list of search paths is specific to a given build context and is
determined as follows:

#. if the ``ocamlfind`` is present in the ``PATH`` of the context, use each line
   in the output of ``ocamlfind printconf path`` as a search path
#. otherwise, if ``opam`` is present in the ``PATH``, use the output of ``opam
   config var lib``
#. otherwise, take the directory where ``ocamlc`` was found, and append
   ``../lib`` to it. For instance if ``ocamlc`` is found in ``/usr/bin``, use
   ``/usr/lib``

.. _running-tests:

Running tests
=============

There are two ways to run tests:

-  ``dune build @runtest``
-  ``dune test`` (or the more explicit ``dune runtest``)

The two commands are equivalent. They will run all the tests defined in the
current directory and its children recursively. You can also run the tests in a
specific sub-directory and its children by using:

-  ``dune build @foo/bar/runtest``
-  ``dune test foo/bar`` (or ``dune runtest foo/bar``)

Watch mode
==========

The ``dune build`` and ``dune runtest`` commands support a ``-w`` (or
``--watch``) flag. When it is passed, dune will perform the action as usual, and
then wait for file changes and rebuild (or rerun the tests). This feature
requires ``inotifywait`` or ``fswatch`` to be installed.

Launching the Toplevel (REPL)
=============================

Dune supports launching a `utop <https://github.com/diml/utop>`__ instance
with locally defined libraries loaded.

.. code:: bash

   $ dune utop <dir> -- <args>

Where ``<dir>`` is a directory under which dune will search (recursively) for
all libraries that will be loaded. ``<args>`` will be passed as arguments to the
utop command itself. For example, ``dune utop lib -- -implicit-bindings`` will
start ``utop`` with the libraries defined in ``lib`` and implicit bindings for
toplevel expressions.

Requirements & Limitations
--------------------------

* utop version >= 2.0 is required for this to work.
* This subcommand only supports loading libraries. Executables aren't supported.
* Libraries that are dependencies of utop itself cannot be loaded. For example
  `Camomile <https://github.com/yoriyuki/Camomile>`__.
* Loading libraries that are defined in different directories into one utop
  instance isn't possible.

Restricting the set of packages
===============================

You can restrict the set of packages from your workspace that dune can see with
the ``--only-packages`` option:

.. code:: bash

    $ dune build --only-packages pkg1,pkg2,... @install

This option acts as if you went through all the dune files and
commented out the stanzas referring to a package that is not in the list
given to ``dune``.

Distributing Projects
=====================

Dune provides support for building and installing your project. However it
doesn't provide helpers for distributing it. It is recommended to use
`dune-release <https://github.com/samoht/dune-release>`__ for this purpose.

The common defaults are that your projects include the following files:

- ``README.md``
- ``CHANGES.md``
- ``LICENSE.md``

And that if your project contains several packages, then all the package names
must be prefixed by the shortest one.

.. _dune-subst:

dune subst
==========

One of the features ``dune-release`` provides is watermarking; it replaces
various strings of the form ``%%ID%%`` in all files of your project
before creating a release tarball or when the package is pinned by the
user using opam.

This is especially interesting for the ``VERSION`` watermark, which gets
replaced by the version obtained from the vcs. For instance if you are using
git, dune-release invokes this command to find out the version:

.. code:: bash

    $ git describe --always --dirty
    1.0+beta9-79-g29e9b37

Projects using dune usually only need dune-release for creating and
publishing releases. However they might still want to substitute the
watermarks when the package is pinned by the user. To help with this,
dune provides the ``subst`` sub-command.

``dune subst`` performs the same substitution ``dune-release`` does
with the default configuration. i.e. calling ``dune subst`` at the
root of your project will rewrite in place all the files in your
project.

More precisely, it replaces all the following watermarks in source files:

- ``NAME``, the name of the project
- ``VERSION``, output of ``git describe --always --dirty``
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

The name of the project is obtained by reading the ``dune-project``
file in the directory where ``dune subst`` is called. The
``dune-project`` file must exist and contain a valid ``(name ...)``
field.

Note that ``dune subst`` is meant to be called from the opam file and
in particular behaves a bit different to other ``dune`` commands. In
particular it doesn't try to detect the root of the workspace and must
be called from the root of the project.

Custom Build Directory
======================

By default dune places all build artifacts in the ``_build`` directory relative
to the user's workspace. However, one can customize this directory by using the
``--build-dir`` flag or the ``DUNE_BUILD_DIR`` environment variable.

.. code:: bash

   $ dune build --build-dir _build-foo

   # this is equivalent to:
   $ DUNE_BUILD_DIR=_build-foo dune build

   # Absolute paths are also allowed
   $ dune build --build-dir /tmp/build foo.exe

Installing a package
====================

Via opam
--------

When releasing a package using Dune in opam there is nothing special
to do.  Dune generates a file called ``<package-name>.install`` at the
root of the project.  This contains a list of files to install and
opam reads it in order to perform the installation.

Manually
--------

When not using opam or when you want to manually install a package,
you can ask Dune to perform the installation via the ``install``
command:

::

    $ dune install [PACKAGE]...

This command takes a list of package names to install.  If no packages
are specified, Dune will install all the packages available in the
workspace.  When several build contexts are specified via a
:ref:`dune-workspace` file, the installation will be performed in all the
build contexts.

Destination directory
---------------------

The ``<prefix>`` directory is determined as follows for a given build
context:

#. if an explicit ``--prefix <path>`` argument is passed, use this path
#. if ``opam`` is present in the ``PATH`` and is configured, use the
   output of ``opam config var prefix``
#. otherwise, take the parent of the directory where ``ocamlc`` was found.

As an exception to this rule, library files might be copied to a
different location. The reason for this is that they often need to be
copied to a particular location for the various build system used in
OCaml projects to find them and this location might be different from
``<prefix>/lib`` on some systems.

Historically, the location where to store OCaml library files was
configured through `findlib
<http://projects.camlcity.org/projects/findlib.html>`__ and the
``ocamlfind`` command line tool was used to both install these files
and locate them. Many Linux distributions or other packaging systems
are using this mechanism to setup where OCaml library files should be
copied.

As a result, if none of ``--libdir`` and ``--prefix`` is passed to ``dune
install`` and ``ocamlfind`` is present in the ``PATH``, then library files will
be copied to the directory reported by ``ocamlfind printconf destdir``. This
ensures that ``dune install`` can be used without opam. When using opam,
``ocamlfind`` is configured to point to the opam directory, so this rule makes
no difference.

Note that ``--prefix`` and ``--libdir`` are only supported if a single build
context is in use.
