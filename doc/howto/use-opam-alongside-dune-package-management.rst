How to Use Opam Alongside Dune Package Management
==================================================

This guide will show you a workflow for gradually adopting Dune Package
Management, while retaining the use of an Opam switch as the default way of
resolving dependencies of a project. It assumes you have a project with
dependencies, and are managing those dependencies with an Opam switch.
By the end of this guide you'll have enabled Dune Package Management for your
project, however by default Dune will still use an Opam switch to resolve your
project's dependencies. By setting an environment variable or passing an extra
argument to commands like ``dune build`` you'll be able to toggle between an
Opam-based workflow and a purely Dune-based workflow.

Create a file named ``dune-workspace`` at the root of your project, with the
following contents:

.. code:: dune

   (lang dune 3.20)

   (pkg disabled)

Since ``dune-workspace`` is the default workspace file, this tells Dune that by
default, don't use its internal package management mechanism to resolve the
project's dependencies, even in the presence of a lockdir.

Now create a second file in the project root named ``dune-workspace.pkg``. The
name of this file isn't important, but conventionally alternative workspace
files begin with the ``dune-workspace.`` prefix. Add the following to this new
file:

.. code:: dune

   (lang dune 3.20)

   (pkg enabled)

With these two files in place, by default Dune will use the specified Opam
switch to resolve your project's dependencies. However, setting the environment
variable ``DUNE_WORKSPACE=dune-workspace.pkg`` will cause Dune to use
its internal package management mechanism to resolve dependencies instead. Unset
the ``DUNE_WORKSPACE`` environment variable to return to an Opam-based workflow.

Alternatively to setting the ``DUNE_WORKSPACE`` environment variable, invoking
commands like ``dune build``, ``dune exec``, and ``dune pkg lock`` with the
argument ``--workspace=dune-workspace.pkg`` also has the effect of changing
which workspace file Dune will use to the one which enables package management.
