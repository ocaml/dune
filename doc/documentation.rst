.. _documentation:

************************
Generating documentation
************************

Prerequisites
=============

Documentation in dune is done courtesy of the odoc_ tool. Therefore, to
generate documentation in dune, you will need to install this tool. This
should likely be done with opam:

::

  $ opam install odoc

Writing documentation
=====================

Documentation comments will be automatically extracted from your OCaml source
files following the syntax described in the section ``Text formatting`` of
the `OCaml manual <http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html>`_.

Additional documentation pages may by attached to a package can be attached
using the :ref:`documentation-stanza` stanza.

Building documentation
======================

Building the documentation using the ``@doc`` alias. Hence, all that is required
to generate documentation for your project is building this alias:

::

  $ dune build @doc

An index page containing links to all the opam packages in your project can be
found in:

::

  $ open _build/default/_doc/_html/index.html

Documentation for private libraries may also be built with:

::

  $ dune build @doc-private

But this libraries will not be in the main html listing above, since they do not
belong to any particular package. But the generated html will still be found in
``_build/default/_doc/_html/<library>``.

Examples
--------

This stanza use attach all the .mld files in the current directory in a project
with a single package.

.. code-block:: lisp

   (documentation)

This stanza will attach three mld files to package foo. The ``mld`` files should
be named ``foo.mld``, ``bar.mld``, and ``baz.mld``

.. code-block:: lisp

   (documentation
    (package foo)
     (mld_files foo bar baz))

This stanza will attach all mld files excluding ``wip.mld`` in the current
directory to the inferred package:

.. code-block:: lisp

   (documentation
    (mld_files :standard \ wip))

.. _odoc-options:

Passing options to Odoc
=======================

.. code-block:: lisp

    (env
     (<profile>
      (odoc <optional-fields>)))

See :ref:`dune-env` for more details on the ``(env ...)`` stanza.
``<optional-fields>`` are:

- ``(warnings <mode>)`` specifies how warnings should be handled.
  ``<mode>`` can be: ``fatal`` or ``nonfatal``.
  The default value is ``nonfatal``.
  This field is available since Dune 2.4.0 and requires Odoc 1.5.0.

.. _odoc: https://github.com/ocaml-doc/odoc
