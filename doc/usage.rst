**********************
Command-Line Interface
**********************

.. TODO(diataxis)

   There are mixed types of contents in this document, including:

   - reference info about finding the root and libraries
   - reference info about the CLI
   - how-to info about overriding what ``dune build`` does

This section describes using ``dune`` from the shell.

.. _finding-root:

Finding the Root
================

The root of the current workspace is determined by looking up a
``dune-workspace`` or ``dune-project`` file in the current directory and its
parent directories. Dune requires at least one of these two files to operate.

If it isn't in the current directory, Dune prints out the root when starting:

.. code:: console

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

A ``dune-workspace`` file is also a configuration file. Dune will read it
unless the ``--workspace`` command line option is used. See
:doc:`/reference/dune-workspace/index` for the syntax of this file. The
scope of ``dune-project`` files is wider than the scope ``dune-workspace``
files. For instance, a ``dune-project`` file may specify the name of the
project which is a universal property of the project, while a
``dune-workspace`` file may specify an opam switch name which is valid only on
a given machine. For this reason, it is common and recommended to commit
``dune-project`` files in repositories, while it is less common to commit
``dune-workspace`` files.


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
:doc:`default alias </reference/aliases/default>`.

Resolution
----------

All targets that Dune knows how to build live in the ``_build`` directory.
Although, some are sometimes copied to the source tree for the need of external
tools. These includes ``<package>.install`` files when either ``-p`` or
``--promote-install-files`` is passed on the command line.

As a result, if you want to ask Dune to produce a particular ``.exe``
file you would have to type:

.. code:: console

    $ dune build _build/default/bin/prog.exe

However, for convenience, when a target on the command line doesn't
start with ``_build``, Dune expands it to the
corresponding target in all the build contexts that Dune knows how to
build. When using ``--verbose``, it prints out the actual set of
targets upon starting:

.. code:: console

    $ dune build bin/prog.exe --verbose
    ...
    Actual targets:
    - _build/default/bin/prog.exe
    - _build/4.03.0/bin/prog.exe
    - _build/4.04.0/bin/prog.exe

If a target starts with the ``@`` sign, it is interpreted as an :term:`alias`.
See :doc:`reference/aliases`.

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

.. code:: console

    $ dune ocaml merlin dump-config

This command prints the distinct configuration of each module present in the
current directory. This directory must be in a Dune workspace and the project
must be already built. The configuration will be encoded as s-expressions, which
are used to communicate with Merlin.

Printing an Approximated ``.merlin``
------------------------------------

It's also possible to print the current folder's configuration in the
Merlin configuration syntax by running the following command:

.. code:: console

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
