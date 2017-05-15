*****
Usage
*****

This section describe usage of Jbuilder from the shell.

Finding the root
================

jbuild-workspace
----------------

The root of the current workspace is determined by looking up a
``jbuild-workspace`` file in the current directory and parent
directories. ``jbuilder`` prints out the root when starting:

.. code:: bash

    $ jbuilder runtest
    Workspace root: /usr/local/home/jdimino/workspaces/public-jane/+share+
    ...

More precisely, it will choose the outermost ancestor directory
containing a ``jbuild-workspace`` file as root. For instance if you are
in ``/home/me/code/myproject/src``, then jbuilder will look for all
these files in order:

-  ``/jbuild-workspace``
-  ``/home/jbuild-workspace``
-  ``/home/me/jbuild-workspace``
-  ``/home/me/code/jbuild-workspace``
-  ``/home/me/code/myproject/jbuild-workspace``
-  ``/home/me/code/myproject/src/jbuild-workspace``

The first entry to match in this list will determine the root. In
practice this means that if you nest your workspaces, Jbuilder will
always use the outermost one.

In addition to determining the root, ``jbuilder`` will read this file as
to setup the configuration of the workspace unless the ``--workspace``
command line option is used. See the *section about workspace
configuration* for the syntax of this file.

jbuild-workspace\*
------------------

In addition to the previous rule, if no ``jbuild-workspace`` file is
found, ``jbuilder`` will look for any file whose name starts with
``jbuild-workspace`` in ancestor directories. For instance
``jbuild-workspace.dev``. If such a file is found, it will mark the root
of the workspace. ``jbuilder`` will however not read its contents.

The rationale for this rule is that it is good practice to have a
``jbuild-workspace.dev`` file at the root of your project.

For quick experiments, simply do this to mark the root:

.. code:: bash

    $ touch jbuild-workspace.here

Current directory
-----------------

If none of the two previous rules appies, i.e. no ancestor directories
have a file whose name starts with ``jbuild-workspace``, then the
current directory will be used as root.

Forcing the root (for scripts)
------------------------------

You can pass the ``--root`` option to ``jbuilder`` to select the root
explicitly. This option is intended for scripts to disable the automatic
lookup.

Notet that when using the ``--root`` option, targets given on the
command line will be interpreted relative to the given root, not
relative to the current directory as this is normally the case.

Interpretation of targets
=========================

This section describes how ``jbuilder`` interprets the targets given on
the command line.

Resolution
----------

Most targets that Jbuilder knows how to build lives in the ``_build``
directory, except for a few:

= ``.merlin`` files

-  ``<package>.install`` files; for the ``default`` context Jbuilder
   knows how generate the install file both in ``_build/default`` and in
   the source tree so that ``opam`` can find it

As a result, if you want to ask ``jbuilder`` to produce a particular
``.exe`` file you would have to type:

.. code:: bash

    $ jbuilder build _build/default/bin/prog.exe

However, for convenience when a target on the command line doesn't start
with ``_build``, ``jbuilder`` will expand it to the corresponding target
in all the build contexts where it knows how to build it. It prints out
the actual set of targets when starting so that you know what is
happening:

.. code:: bash

    $ jbuilder build bin/prog.exe
    ...
    Actual targets:
    - _build/default/bin/prog.exe
    - _build/4.03.0/bin/prog.exe
    - _build/4.04.0/bin/prog.exe

Aliases
-------

Targets starting with a ``@`` are interpreted as aliases. For instance
``@src/runtest`` means the alias ``src/runtest``. If you want to refer
to a target starting with a ``@``, simply write: ``./@foo``.

Note that an alias not pointing to the ``_build`` directory always
depends on all the corresponding aliases in build contexts.

So for instance:

-  ``jbuilder build @_build/foo/runtest`` will run the tests only for
   the ``foo`` build context
-  ``jbuilder build @runtest`` will run the tests for all build contexts

Finding external libraries
==========================

When a library is not available in the workspace, jbuilder will look it
up in the installed world, and expect it to be already compiled.

It looks up external libraries using a specific list of search pathes. A
list of search pathes is specific to a given build context and is
determined as follow:

#. if the ``ocamlfind`` is present in the ``PATH`` of the context, use
   each line in the output of ``ocamlfind printconf path`` as a search
   path
#. otherwise, if ``opam`` is present in the ``PATH``, use the outout of
   ``opam config var lib``
#. otherwise, take the directory where ``ocamlc`` was found, and append
   ``../lib`` to it. For instance if ``ocamlc`` is found in
   ``/usr/bin``, use ``/usr/lib``

Running tests
-------------

There are two ways to run tests:

-  ``jbuilder build @runtest``
-  ``jbuilder runtest``

The two commands are equivalent. They will run all the tests defined in
the current directory and its children recursively. You can also run the
tests in a specific sub-directory and its children by using:

-  ``jbuilder build @foo/bar/runtest``
-  ``jbuidler runtest foo/bar``

Restricting the set of packages
===============================

You can restrict the set of packages from your workspace that Jbuilder
can see with the ``--only-packages`` option:

.. code:: bash

    $ jbuilder build --only-packages pkg1,pkg2,... @install

This option acts as if you went through all the jbuild files and
commented out the stanzas refering to a package that is not in the list
given to ``jbuilder``.

Invocation from opam
====================

You should set the ``build:`` field of your ``<package>.opam`` file as
follows:

::

    build: [["jbuilder" "build" "-p" name "-j" jobs]]

``-p pkg`` is a shorthand for ``--root . --only-packages pkg``. ``-p``
is the short version of ``--for-release-of-packages``.

This has the following effects:

-  it tells jbuilder to build everything that is installable and to
   ignore packages other than ``name`` defined in your project
-  it sets the root to prevent jbuilder from looking it up
-  it uses whatever concurrency option opam provides

Note that ``name`` and ``jobs`` are variables expanded by opam. ``name``
expands to the package name and ``jobs`` to the number of jobs available
to build the package.

Tests
=====

To setup the building and running of tests in opam, add this line to
your ``<package>.opam`` file:

::

    build-test: [["jbuilder" "runtest" "-p" name "-j" jobs]]

Installation
============

Installing a package means copying the build artifacts from the build
directory to the installed word.

When installing via opam, you don't need to worry about this step:
jbuilder generates a ``<package>.install`` file that opam will
automatically read to handle installation.

However, when not using opam or doing local development, you can use
jbuilder to install the artifacts by hands. To do that, use the
``install`` command:

::

    $ jbuilder install [PACKAGE]...

without an argument, it will install all the packages available in the
workspace. With a specific list of packages, it will only install these
packages. If several build contexts are configured, the installation
will be performed for all of them.

Note that ``jbuilder install`` is a thin wrapper around the
``opam-installer`` tool, so you will need to install this tool in order
to be able to use ``jbuilder install``.

Destination
-----------

The place where the build artifacts are copied, usually referred as
**prefix**, is determined as follow for a given build context:

#. if an explicit ``--prefix <path>`` argument is passed, use this path
#. if ``opam`` is present in the ``PATH``, use the output of ``opam config var
   prefix``
#. otherwise, take the directory where ``ocamlc`` was found, and append
   ``../lib`` to it. For instance if ``ocamlc`` is found in ``/usr/bin``, use
   ``/usr``

Note that ``--prefix`` is only supported if a single build context is in
use.

Workspace configuration
=======================

By default, a workspace has only one build context named ``default``
which correspond to the environment in which ``jbuilder`` is run. You
can define more contexts by writing a ``jbuild-workspace`` file.

You can point ``jbuilder`` to an explicit ``jbuild-workspace`` file with
the ``--workspace`` option. For instance it is good practice to write a
``jbuild-workspace.dev`` in your project with all the version of OCaml
your projects support. This way developpers can tests that the code
builds with all version of OCaml by simply running:

.. code:: bash

    $ jbuilder build --workspace jbuild-workspace.dev @install @runtest

jbuild-workspace
----------------

The ``jbuild-workspace`` file uses the S-expression syntax. This is what
a typical ``jbuild-workspace`` file looks like:

.. code:: scheme

    (context ((switch 4.02.3)))
    (context ((switch 4.03.0)))
    (context ((switch 4.04.0)))

The rest of this section describe the stanzas available.

context
~~~~~~~

The ``(context ...)`` stanza declares a build context. The argument
can be either ``default`` for the default build context or can be the
description of an opam switch, as follows:

.. code:: scheme

    (context ((switch <opam-switch-name>)
              <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the name of the subdirectory of ``_build``
   where the artifacts for this build context will be stored

-  ``(root <opam-root>)`` is the opam root. By default it will take
   the opam root defined by the environment in which ``jbuilder`` is
   run which is usually ``~/.opam``

-  ``(merlin)`` instructs Jbuilder to generate the ``.merlin`` files
   from this context. There can be at most one build context with a
   ``(merlin)`` field. If no build context has a ``(merlin)`` field,
   the selected context for ``merlin`` will be ``(context default)``
   if present. Otherwise Jbuilder won't generate ``.merlin`` files

Building JavaScript with js_of_ocaml
====================================

Jbuilder knows how to generate a JavaScript version of an executable
(``<name>.bc.js``) using the js_of_ocaml compiler (the ``js_of_ocaml-compiler``
opam package must be installed).

It supports two modes of compilation:

- Direct compilation of a bytecode program to JavaScript. This mode allows
  js_of_ocaml to perform whole program deadcode elimination and whole program
  inlining.
- Separate compilation, where compilation units are compiled to JavaScript
  separately and then linked together. This mode is useful during development as
  it builds more quickly.

The separate compilation mode will be selected when passing ``--dev`` to
jbuilder. There is currently no other way to control this behaviour.

See the *section about js_of_ocalm* for passing custom flags to the js_of_ocaml
compiler

Using topkg with jbuilder
=========================

Jbuilder provides suport for building and installing your project.
However it doesn't provides helpers for distributing it. It is
recommemded to use `Topkg <https://github.com/dbuenzli/topkg>`__ for
this purpose.

The `topkg-jbuilder <https://github.com/diml/topkg-jbuilder>`__ project
provides helpers for using Topkg in a Jbuilder project.
