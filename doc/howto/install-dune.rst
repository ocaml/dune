How to Install Dune
===================

Dune is available as an Opam package. First, make sure that Opam is installed:

  .. code:: console

   $ opam --version
   2.1.5

Any version higher than 2.0.0 is supported, though preferably at least 2.1.0.

If Opam is not available, follow `the official instructions on the Opam website
<https://opam.ocaml.org/doc/Install.html>`_ to install it and then run its
global setup with ``opam init``.

.. note::

   Opam requires a "shell hook" to work properly. Make sure to set it up
   correctly during ``opam init``. Otherwise you will have to run ``eval $(opam
   env)`` every time you create an Opam switch or change directory.

Then, you can install Dune in an Opam switch using the following command:

.. code:: console

  $ opam install dune

After the command completes, the following should display a version number:

.. code:: console

  $ dune --version
  3.12.1

.. note::

   In most cases, when using Opam you will not need to install Dune by hand.
   Installing the project's dependencies will install it in the Opam switch.
