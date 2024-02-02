dune-workspace
==============

By default, a workspace has only one build context named ``default`` which
corresponds to the environment, in which ``dune`` is run. You can define more
contexts by writing a ``dune-workspace`` file.

You can point Dune to an explicit ``dune-workspace`` file with the
``--workspace`` option. For instance, it's good practice to write a
``dune-workspace.dev`` in your project with all the OCaml versions your projects
support, so developers can test that the code builds with all OCaml versions by
simply running:

.. code:: console

    $ dune build --workspace dune-workspace.dev @all @runtest

The ``dune-workspace`` file uses the S-expression syntax. This is what a typical
``dune-workspace`` file looks like:

.. code:: dune

    (lang dune 3.14)
    (context (opam (switch 4.07.1)))
    (context (opam (switch 4.08.1)))
    (context (opam (switch 4.11.1)))

The rest of this section describe the stanzas available.

Note that an empty ``dune-workspace`` file is interpreted the same as one
containing exactly:

.. code:: dune

    (lang dune 3.2)
    (context default)

This allows you to use an empty ``dune-workspace`` file to mark the root of your
project.

.. toctree::
   
  config
  context
  env
  profile
