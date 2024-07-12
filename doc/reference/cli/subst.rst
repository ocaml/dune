.. _dune-subst:

subst - Substitute Watermarks in Source Files
=============================================

One of the features ``dune-release`` provides is watermarking; it replaces
various strings of the form ``%%ID%%`` in all your project files
before creating a release tarball or when the opam user pins the package.

This is especially interesting for the ``VERSION`` watermark, which gets
replaced by the version obtained from the Version-Control System (VCS). For instance, if you're using
Git, ``dune-release`` invokes this command to find out the version:

.. code:: console

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
