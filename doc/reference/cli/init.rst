.. _initializing_components:

init - Initializing Components
==============================

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

.. code:: console

   $ dune init proj myproj --libs base,cmdliner --inline-tests --ppx ppx_inline_test

This creates a new directory called ``myproj``, including subdirectories and
``dune`` files for library, executable, and test components. Each component's
``dune`` file will also include the declarations required for the given
dependencies.

This is the quickest way to get a basic ``dune`` project up and building.

Initializing an Executable
--------------------------

To add a new executable to a ``dune`` file in the current directory
(creating the file if necessary), run

.. code:: console

    $ dune init exe myexe --libs base,containers,notty --ppx ppx_deriving

This will add the following stanza to the ``dune`` file:

.. code:: dune

    (executable
     (name main)
     (libraries base containers notty)
     (preprocess
      (pps ppx_deriving)))

Initializing a Library
----------------------

Run the following command to create a new directory ``src``, initialized as a library:

.. code:: console

    $ dune init lib mylib src --libs core --inline-tests --public

This will ensure the file ``./src/dune`` contains the below stanza (creating
the file and directory, if necessary):

.. code:: dune

    (library
     (public_name mylib)
     (inline_tests)
     (name mylib)
     (libraries core)
     (preprocess
      (pps ppx_inline_tests)))

Initializing Components in a Specified Directory
------------------------------------------------

All ``init`` subcommands take an optional ``PATH`` argument, which should be a
path to a directory. When supplied, the component will be created in the
specified directory. E.g., to initialize a project in the current working
directory, run

.. code:: console

    $ dune init proj my_proj .

To initialize a project in a directory in some nested path, run

.. code:: console

    $ dune init proj my_proj path/to/my/project

If the specified directory does not already exist, it will be created.

Learning More About the ``init`` Commands
-----------------------------------------

Consult the manual page using the ```dune init --help`` command for more
details.
