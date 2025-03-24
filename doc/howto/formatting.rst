How to Set up Automatic Formatting
==================================

This guide will show you how to configure Dune so that it can check the formatting
of your source code.

Formatting is defined per project. This ensures that if a project is reused
elsewhere, its formatting configuration will not interfere.

Setting Up the Environment
--------------------------

First, let's open the ``dune-project`` file. Make sure that the version
specified in ``(lang dune X.Y)`` is at least ``2.0``. Most formatting
configuration happens in that file. If you want to format OCaml sources and
``dune`` files, you don't have anything to add. Otherwise, refer to the
:doc:`/reference/dune-project/formatting` stanza.

Next we need to install some code formatting tools. For OCaml code, this means
installing OCamlFormat_ with ``opam install ocamlformat``. Formatting ``dune``
files is built into Dune and does not require any extra tools. For Reason code,
this uses the ``refmt`` tool which is already installed if you are using Reason
syntax in your project. If your project uses a :term:`dialect`, a specific tool
might be required.

.. _ocamlformat: https://github.com/ocaml-ppx/ocamlformat

Using OCamlFormat requires some configuration. Take note of the version
returned by ``ocamlformat --version`` (let's name that ``X.Y.Z``) and create an
``.ocamlformat`` file in the same directory as ``dune-project`` with the
following contents:

.. code::

   version=X.Y.Z
   profile=default

The ``version`` line is checked by OCamlFormat and ensures that everybody
contributing to the project uses the same version.

Note that you do not have to add ``ocamlformat`` to your opam files.

Running the Formatters
----------------------

Run the ``dune build @fmt`` command. It will format the source files in the
corresponding project and display the differences:

.. code:: console

    $ dune build @fmt
    --- hello.ml
    +++ hello.ml.formatted
    @@ -1,3 +1 @@
    -let () =
    -  print_endline
    -    "hello, world"
    +let () = print_endline "hello, world"

Then it's possible to accept the correction by calling ``dune promote`` to
replace the source files with the corrected versions.

.. code:: console

    $ dune promote
    Promoting _build/default/hello.ml.formatted to hello.ml.

As usual with promotion, it's possible to combine these two steps by running
``dune build @fmt --auto-promote``. This command can also be shortened to
``dune fmt``. See :doc:`../concepts/promotion` for more details.

Setting Up Your CI
------------------

To check formatting in CI, the precise set up depends on the CI system used,
but in general it is easier to set up a dedicated job that just installs
``dune`` and the formatting tools, rather than doing that as part of the jobs
that run tests.

If you use `ocaml-ci`_, you have nothing to do: a formatting job is set up
automatically.

If you use `setup-ocaml`_, you can use the `lint-fmt` extend listed in the
README file.

.. _ocaml-ci: https://ocaml.ci.dev/
.. _setup-ocaml: https://github.com/ocaml/setup-ocaml
