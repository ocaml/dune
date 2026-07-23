How to Organize Multiple Executables in Different Directories
=============================================================

A common question when a project grows beyond a single program is: "I want to
build several executables, each living in its own directory. How do I set that
up?"

Dune does not impose a particular layout on your project. You are free to place
your executables in whatever directories make sense to you. The key idea is
that you *describe* each directory with a ``dune`` file, and Dune assembles the
whole project from those descriptions.

The Mental Model
----------------

Three rules are enough to organize almost any project:

- A project has a single ``dune-project`` file at its root. It records
  project-wide information, most importantly the version of the Dune language
  in use.

- Every directory that contains something to build has its own ``dune`` file
  describing what is in that directory.

- An executable is defined by an :doc:`executable
  </reference/dune/executable>` stanza in the ``dune`` file of the directory
  where its sources live.

So to have executables in several directories, you simply put a ``dune`` file
with an ``executable`` stanza in each of those directories. Nothing forces them
to sit together, and there is no central list of executables to maintain.

Consider a project with a ``server`` and a ``client``, each in its own
directory:

.. code:: text

   myproject/
   ├── dune-project
   ├── client/
   │   ├── dune
   │   └── client.ml
   └── server/
       ├── dune
       └── server.ml

The ``dune-project`` File
-------------------------

There is exactly one ``dune-project`` file, and it lives at the root of the
project. The directories below it do not need their own. At a minimum it
declares the language version:

.. code:: dune

   (lang dune {{latest}})

Executables in Multiple Subdirectories
--------------------------------------

Each executable is described where its sources are. The ``client/dune`` file
declares the client:

.. code:: dune

   (executable
    (name client))

And ``server/dune`` declares the server in the same way:

.. code:: dune

   (executable
    (name server))

With the source files in place, for example ``client/client.ml``:

.. code:: ocaml

   let () = print_endline "client starting"

Dune builds both executables from the root of the project:

.. code:: console

   $ dune build

You run each one by giving its path relative to the project root, using the
``.exe`` extension regardless of platform:

.. code:: console

   $ dune exec client/client.exe
   client starting

   $ dune exec server/server.exe
   server starting

The directory names (``client`` and ``server``) are entirely your choice, and
the directories can be nested as deeply as you like. Dune discovers them by
reading the ``dune`` files it finds while traversing the tree.

Sharing Code Between Executables
--------------------------------

As soon as two executables need to share code, move that code into a library.
A library is just another directory with a ``dune`` file, this time containing
a :doc:`library </reference/dune/library>` stanza. Place it wherever you like;
a ``lib/`` directory is a common convention:

.. code:: text

   myproject/
   ├── dune-project
   ├── client/
   │   ├── dune
   │   └── client.ml
   ├── server/
   │   ├── dune
   │   └── server.ml
   └── lib/
       └── protocol/
           ├── dune
           └── protocol.ml

The ``lib/protocol/dune`` file defines the shared library:

.. code:: dune

   (library
    (name protocol))

Its modules are written in that directory, for example
``lib/protocol/protocol.ml``:

.. code:: ocaml

   let greeting = "hello from protocol"

Each executable that wants the library lists it under ``libraries``. Note that
a library is referred to by the ``name`` from its stanza, not by its directory
path, so it does not matter where in the tree the library lives. The updated
``client/dune`` becomes:

.. code:: dune

   (executable
    (name client)
    (libraries protocol))

The client can now use the library's modules directly:

.. code:: ocaml

   let () = print_endline Protocol.greeting

The same ``(libraries protocol)`` line added to ``server/dune`` gives the
server access to the very same code. See :doc:`/reference/library-dependencies`
for the full syntax of the ``libraries`` field.

A Note on ``dune init``
-----------------------

The ``dune init`` command can scaffold projects, executables, and libraries for
you, which is handy when you are starting out. It is only a convenience,
though: it writes the same ``dune-project`` and ``dune`` files shown above.
Once you understand the layout, you can create and arrange these files by hand
in whatever structure suits your project.
