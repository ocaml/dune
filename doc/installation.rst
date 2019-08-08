*********************************
Installing packages on the system
*********************************

Installation is the process of copying freshly built libraries,
binaries and other files from the build directory to the system.  Dune
offers two way of doing this: via opam or directly via the ``install``
command.  In particular, the installation model implemented by Dune
was copied from opam. Opam is the standard OCaml package manager.

In both cases, Dune only know how to install whole packages.  A
package being a collection of executables, libraries and other files.
In this section, we will describe how to define a package, how to
"attach" various elements to it and how to proceed with installing it
on the system.

Declaring a package
===================

To declare a package, simply add a ``package`` stanza to your
``dune-project`` file:

.. code:: scheme

          (package
           (name mypackage)
           (synopsis "My first Dune package!")
           (description "\| This is my first attempt at creating
                        "\| a project with Dune.
          ))

Once you have done this, Dune will know about the package named
``mypackage`` and you will be able to attach various elements to it.
The ``package`` stanza accepts more fields, such as dependencies.

Note that package names are in a global namespace so the name you choose must
be universally unique.  In particular, package managers never allow to
release two packages with the same name.

.. TODO: describe this more in details

In older projects using Dune, packages were defined by the presence of
a file called ``<package-name>.opam`` at the root of the project.
However, it is not recommended to use this method in new projects as
we expect to deprecate it in the future.  The right way to define a
package is with a ``package`` stanza in the ``dune-project`` file.

Attaching elements to a package
===============================

Attaching an element to a package means declaring to Dune that this
element is part of the said package.  The method to attach an element
to a package depends on the kind of the element.  In this sub-section
we will go through the various kinds of elements and describe how to
attach each of them to a package.

In the rest of this section, ``<prefix>`` refers to the directory in
which the user chooses to install packages.  When installing via opam,
it is opam who sets this directory.  When calling ``dune install``,
the installation directory is either guessed or can be manually
specified by the user.  This is described more in detail in the last
section of this page.

Libraries
---------

In order to attach a library to a package all you need to do is add a
``public_name`` field to your library.  This is the name that external
users of your libraries must use in order to refer to it.  Dune
requires that the public name of a library is either the name of the
package it is part of or start with the package name followed by a dot
character.

For instance:

.. code:: scheme

          (library
           (name mylib)
           (public_name mypackage.mylib))

After you have added a public name to a library, Dune will know to
install it as part of the package it is attached to.  Dune installs
the library files in a directory ``<prefix>/lib/<package-name>``.

If the library name contains dots, the full directory in which the
library files are installed is ``lib/<comp1>/<comp2/.../<compn>``
where ``<comp1>``, ``<comp2>``, ... ``<compn>`` are the dot separated
component of the public library name.  By definition, ``<comp1>`` is
always the package name.

Executables
-----------

Similarly to libraries, to attach an executable to a package simply
add a ``public_name`` field to your ``executable`` stanza, or a
``public_names`` field for ``executables`` stanzas.  The name that
goes in there is the name under which the executables will be
available through the ``PATH`` once installed, i.e. the name users
will need to type in there shell to execute the program.  Because Dune
cannot guess which package an executable is part of from its public
name, you also need to add a ``package`` field unless the project
contains a single package, in which case the executable will be
attached to this package.

For instance:

.. code:: scheme

          (executable
           (name main)
           (public_name myprog)
           (package mypackage))

Once ``mypackage`` is installed on the system, the user will be able
to type the following in their shell:

::

   $ myprog

to execute the program.

Other files
-----------

For all other kinds of elements, you need to attach them manually via
an ``install`` stanza.  The install stanza takes three informations:

- the list of files the install
- the package to attach these files to. This field is optional if your
  project contains a single package
- the section in which the files will be installed

For instance:

.. code::

   (install
    (files hello.txt)
    (section share)
    (package mypackage))

Indicate that the file ``hello.txt`` in the current directory is to be
installed in ``<prefix>/share/mypacakge``.

The following sections are available:

- ``lib`` installs to ``<prefix>/lib/<pkgname>/``
- ``lib_root`` installs to ``<prefix>/lib/``
- ``libexec`` installs to ``<prefix>/lib/<pkgname>/`` with the
   executable bit set
- ``libexec_root`` installs to ``<prefix>/lib/`` with the executable
   bit set
- ``bin`` installs to ``<prefix>/bin/`` with the executable bit set
- ``sbin`` installs to ``<prefix>/sbin/`` with the executable bit set
- ``toplevel`` installs to ``<prefix>/lib/toplevel/``
- ``share`` installs to ``<prefix>/share/<pkgname>/``
- ``share_root`` installs to ``<prefix>/share/``
- ``etc`` installs to ``<prefix>/etc/<pkgname>/``
- ``doc`` installs to ``<prefix>/doc/<pkgname>/``
- ``stublibs`` installs to ``<prefix>/lib/stublibs/`` with the
   executable bit set
- ``man`` installs relative to ``<prefix>/man`` with the destination
   directory extracted from the extension of the source file (so that
   installing ``foo.1`` is equivalent to a destination of
   ``man1/foo.1``)
- ``misc`` requires files to specify an absolute destination, and the
   user will be prompted before the installation when it is done via
   opam. Only use this for advanced cases.

Normally, Dune uses the basename of the file to install to determine
the name of the file once installed.  However, you can change that
fact by using the form ``(<filename> as <destination>)`` in the
``files`` field. For instance, to install a file ``mylib.el`` as
``<prefix>/emacs/site-lisp/mylib.el`` you must write the following:

.. code:: scheme

    (install
     (section share_root)
     (files   (mylib.el as emacs/site-lisp/mylib.el)))

Installing a package
====================

Via opam
--------

When releasing a package using Dune in opam there is nothing special
to do.  Dune generates a file called ``<package-name>.opam`` at the
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
