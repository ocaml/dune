.. _formatting-main:

********************
Automatic Formatting
********************

Dune can be set up to run automatic formatters for source code.

It can use OCamlformat_ to format OCaml source code (``*.ml`` and ``*.mli``
files) and refmt_ to format Reason source code (``*.re`` and ``*.rei`` files).

Furthermore it can be used to format code of any defined dialect (see
:ref:`dialect`).

.. _ocamlformat: https://github.com/ocaml-ppx/ocamlformat
.. _refmt: https://github.com/facebook/reason/tree/master/src/refmt

Configuring Automatic Formatting (Dune 2.0)
===========================================

If using ``(lang dune 2.0)``, there is nothing to setup in Dune, as formatting will
be set up by default. However, OCamlformat_ will still refuse to format sources
without an ``.ocamlformat`` file present in the project root.

By default, formatting will be enabled for all languages and dialects present in
the project that Dune knows about. This is not always desirable. For example, if
in a mixed Reason/OCaml project, one only wants to format the Reason files to
avoid pulling OCamlformat_ as a dependency.

It is possible to restrict the languages considered for formatting or disable it
altogether by using the :ref:`formatting` stanza.


Formatting a Project
====================

When this feature is active, an alias named ``fmt`` is defined. When built, it
will format the source files in the corresponding project and display the
differences:

.. code::

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

.. code::

    $ dune promote
    Promoting _build/default/hello.ml.formatted to hello.ml.

As usual with promotion, it's possible to combine these two steps by running
``dune build @fmt --auto-promote``.

Starting with Dune 3.2.0, you can also run ``dune fmt`` which is equivalent to
``dune build @fmt --auto-promote``.

Enabling and Configuring Automatic Formatting (Dune 1.x)
========================================================

.. note:: This section applies only to projects with ``(lang dune 1.x)``.

In ``(lang dune 1.x)``, there is no default formatting. This feature is
enabled by adding the following to the ``dune-project`` file:

.. code:: dune

    (using fmt 1.2)

Languages can be configured using the following syntax:

.. code:: dune

    (using fmt 1.2 (enabled_for reason))

Version History
===============

(lang dune 2.0)
---------------

* Formatting is enabled by default.

(using fmt 1.2)
---------------

* Format dialects (see :ref:`dialect`).

(using fmt 1.1)
---------------

* Format Dune files.

(using fmt 1.0)
---------------

* Format OCaml (using ocamlformat_) and Reason (using refmt_) source code.
