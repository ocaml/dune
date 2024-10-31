install - Manually Installing a Package
=======================================

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

.. code:: console

    $ dune install [PACKAGE]...

This command takes a list of package names to install.  If no packages
are specified, Dune will install all available packages in the
workspace.  When several build contexts are specified via a
:doc:`/reference/dune-workspace/index` file, Dune performs the
installation in all the build contexts.

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
