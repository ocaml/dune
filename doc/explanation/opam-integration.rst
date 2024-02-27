###############################
 How Dune integrates with opam
###############################

.. highlight:: opam

When instructed to do so (see :doc:`../howto/opam-file-generation`),
Dune generates opam files with the following instructions:

.. code::

   build: [
     ["dune" "subst"] {dev}
     [
       "dune"
       "build"
       "-p"
       name
       "-j"
       jobs
       "@install"
       "@runtest" {with-test}
       "@doc" {with-doc}
     ]
   ]

Let's see what this means in detail.

**************
 Substitution
**************

The first step is to call ``dune subst``, but only if the ``{dev}`` opam
variable is set. This variable is only set when the package is pinned.
This means that :ref:`dune-subst` does not run for released versions,
but it does for development versions.

This is not a problem since released versions should have a
``(version)`` field set in ``dune-project``, and :term:`placeholder
substitution` should have been performed. dune-release_ takes care of
these steps.

.. _dune-release: https://github.com/tarides/dune-release

****************
 Opam Variables
****************

In the second command line, ``name`` is a variable that evaluates to the
name of the package being built, and ``jobs`` is a variable that
corresponds to the number of commands to run in parallel.

*******************
 What ``-p`` Means
*******************

The ``-p`` flag, shorthand for ``--release-of-packages``, is Dune's
public interface to set up the options for an opam build. The exact
semantics may change, but as of Dune 3.8 it is equivalent to the
combination of:

-  ``--root .``: set the :term:`root` to prevent Dune from :ref:`looking
   it up <finding-root>`.

-  ``--only-packages name``: ignore packages other than ``name`` defined
   in the project.

-  ``--profile release``: set the :term:`build profile` to ``release``.
   In particular, this ensures that warnings are not fatal.

-  ``--ignore-promoted-rules``: silently ignores all rules with ``(mode
   promote)``.

-  ``--default-target @install``: make sure that ``dune build`` with no
   target argument builds ``@install``, not ``@@default`` (this is not
   used in the opam integration since an explicit target is passed)

-  ``--no-config``: do not load the configuration file in the user's
   home directory.

-  ``--always-show-command-line``: ensures that the programs executed by
   Dune end up in the opam logs.

-  ``--promote-install-files``: ensures that ``*.install`` files are
   present in the source tree after the build.

-  ``--require-dune-project-file``: fail if ``dune-project`` is not
   present. In some previous Dune versions, ``dune-project`` could be
   generated when it is not present. This is not desirable with opam
   since the version the package has been prepared with is not known.

****************************
 The Targets We're Building
****************************

The targets are specified as:

.. code::

   "@install"
   "@runtest" {with-test}
   "@doc" {with-doc}

The ``{with- }`` syntax is an opam filter. It means that the string
before is present or not depending on the opam variable. These
variables, in turn, are set depending on the opam configuration.

Concretely, in the next table, if the opam command on the left is
executed, the Dune target on the right will be built:

.. list-table::
   :header-rows: 1

   -  -  opam command
      -  Dune target
   -  -  ``opam install pkg``
      -  ``@install``
   -  -  ``opam install pkg --with-test``
      -  ``@install @runtest``
   -  -  ``opam install pkg --with-test --with-doc``
      -  ``@install @runtest @doc``

This filtering mechanism is also used to declare dependencies. If a
package is using ``lwt`` and ``alcotest``, but the latter only in its
test suite, its ``depends:`` field is:

.. code::

   "lwt"
   "alcotest" {with-test}

This is expanded to just ``"lwt"`` in ``opam install pkg``, but to
``"lwt" "alcotest"`` in ``opam install pkg --with-test``.

The meaning of these :term:`aliases <alias>` is the following:

-  ``@install`` depends on all the ``*.install`` files in the project.
   In turn, these depend on all the installable files (libraries and
   executables with a public name and files that are manually installed
   through ``(install)`` stanzas).

-  ``@runtest`` is the alias to which all tests are attached, including
   ``(test)`` stanzas. ``dune build @runtest`` is equivalent to ``dune
   runtest``.

-  ``@doc`` executes ``odoc`` to create HTML docs under ``_build``.

*****************************
 What Opam Expects From Dune
*****************************

Given this ``build:`` lines and the fact that there is no ``install:``
line, what happens is the following:

-  Opam executes ``dune subst``, if the package is being pinned.

-  Opam executes the build instruction, usually just ``dune build -p pkg
   @install``

-  This Dune command builds all the installable files and creates a
   ``pkg.install`` file.

-  This file contains the paths to built files (somewhere in the
   ``_build`` directory) and the opam sections they should be installed
   in.

-  Opam interprets this file and copies the built files to their
   destination. The install file is also used as a manifest of which
   files belong to which package, which is used when uninstalling the
   package.
