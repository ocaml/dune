Package Specification
=====================

.. TODO(diataxis)
   - reference: packages
   - howto: preparing an opam package
   - tutorial: from zero to opam

Installation is the process of copying freshly built libraries,
binaries, and other files from the build directory to the system. Dune
offers two ways of doing this: via opam or directly via the ``install``
command. In particular, the installation model implemented by Dune
was copied from opam. Opam is the standard OCaml package manager.

In both cases, Dune only know how to install whole packages. A
package being a collection of executables, libraries, and other files.
In this section, we'll describe how to define a package, how to
"attach" various elements to it, and how to proceed with installing it
on the system.

.. _declaring-a-package:

Declaring a Package
-------------------

To declare a package, simply add a ``package`` stanza to your
``dune-project`` file:

.. code:: dune

          (package
           (name mypackage)
           (synopsis "My first Dune package!")
           (description "\| This is my first attempt at creating
                        "\| a project with Dune.
          ))

Once you have done this, Dune will know about the package named
``mypackage`` and you will be able to attach various elements to it.
The ``package`` stanza accepts more fields, such as dependencies.

Note that package names are in a global namespace, so the name you choose must
be universally unique. In particular, package managers never allow users to
release two packages with the same name.

.. TODO: describe this more in details

In older projects using Dune, packages were defined by manually writing a file
called ``<package-name>.opam`` at the root of the project. However, it's not
recommended to use this method in new projects, as we expect to deprecate it in
the future. The right way to define a package is with a ``package`` stanza in
the ``dune-project`` file.

See :doc:`../howto/opam-file-generation` for instructions on configuring Dune
to automatically generate ``.opam`` files based on the ``package`` stanzas.

Attaching Elements to a Package
-------------------------------

Attaching an element to a package means declaring to Dune that this
element is part of the said package. The method to attach an element
to a package depends on the kind of the element. In this subsection,
we will go through the various kinds of elements and describe how to
attach each of them to a package.

In the rest of this section, ``<prefix>`` refers to the directory in
which the user chooses to install packages. When installing via opam,
it's opam that sets this directory. When calling ``dune install``,
the installation directory is either guessed or can be manually
specified by the user. Defaults directories which replace guessing
can be set during the compilation of dune.

Sites of a Package
------------------

When packages need additional resources outside their binary, their location
could be hard to find. Moreover, some packages could add resources to another
package, e.g., in the case of plugins. These locations are called sites in
Dune. One package can define them. During execution, one site corresponds to a
list of directories. They are like layers, and the first directories have a higher
priority. Examples and precisions are available at :ref:`sites`.


Libraries
^^^^^^^^^

In order to attach a library to a package, merely add a
``public_name`` field to your library. This is the name that external
users of your libraries must use in order to refer to it. Dune
requires that a library's public name is either the name of the
package it is part of or start with the package name followed by a dot
character.

For instance:

.. code:: dune

   (library
    (name mylib)
    (public_name mypackage.mylib))

After you have added a public name to a library, Dune will know to
install it as part of the package it is attached to. Dune installs
the library files in a directory ``<prefix>/lib/<package-name>``.

If the library name contains dots, the full directory in which the
library files are installed is ``lib/<comp1>/<comp2/.../<compn>``,
where ``<comp1>``, ``<comp2>``, ... ``<compn>`` are the dot-separated
component of the public library name. By definition, ``<comp1>`` is
always the package name.

Executables
^^^^^^^^^^^

Similar to libraries, to attach an executable to a package simply
add a ``public_name`` field to your ``executable`` stanza or a
``public_names`` field for ``executables`` stanzas. Designate this
name to match the available executables through the installed ``PATH``
(i.e., the name users must type in their shell to execute
the program), because Dune cannot guess an executable's relevant package
from its public name. It's also necessary to add a ``package`` field
unless the project contains a single package, in which case the executable
will be attached to this package.

For instance:

.. code:: dune

          (executable
           (name main)
           (public_name myprog)
           (package mypackage))

Once ``mypackage`` is installed on the system, the user will be able
to type the following in their shell:

.. code:: console

   $ myprog

to execute the program.

Other Files
^^^^^^^^^^^

For all other kinds of elements, you must attach them manually via
an :doc:`/reference/dune/install` stanza.
