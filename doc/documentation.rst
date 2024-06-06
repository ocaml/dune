.. _documentation:

************************
Generating Documentation
************************

.. TODO(diataxis)

   Split between:

   - A "generating API documentation" how-to guide
   - Some reference documentation

Prerequisites
=============

Documentation in Dune is done courtesy of the odoc_ tool. Therefore, to
generate documentation in Dune, you will need to install this tool. This
should be done with opam:

::

  $ opam install odoc

Writing Documentation
=====================

Documentation comments will be automatically extracted from your OCaml source
files following the syntax described in the section ``Text formatting`` of
the `OCaml manual <http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html>`_.

Additional documentation pages may be attached to a package using the
:doc:`/reference/dune/documentation` stanza.

Building Documentation
======================

To generate documentation using the :doc:`/reference/aliases/doc` alias, all
that's required to is to build this alias:

.. code:: console

  $ dune build @doc

An index page containing links to all the opam packages in your project can be
found in:

.. code:: console

  $ open _build/default/_doc/_html/index.html

Documentation for private libraries may also be built with
:doc:`/reference/aliases/doc-private`:

.. code:: console

  $ dune build @doc-private

But these libraries will not be in the main HTML listing above, since they
don't belong to any particular package, but the generated HTML will still be
found in ``_build/default/_doc/_html/<library>``.


Documentation Stanza: Examples
------------------------------

The :doc:`/reference/dune/documentation` stanza will attach all the
``.mld`` files in the current directory in a project with a single package.

.. code-block:: dune

   (documentation)

This stanza will attach three ``.mld`` files to package ``foo``. The ``.mld`` files should
be named ``foo.mld``, ``bar.mld``, and ``baz.mld``

.. code-block:: dune

   (documentation
    (package foo)
     (mld_files foo bar baz))

This stanza will attach all ``.mld`` files to the inferred package, 
excluding ``wip.mld``, in the current directory:

.. code-block:: dune

   (documentation
    (mld_files :standard \ wip))

All ``.mld`` files attached to a package will be included in the generated
``.install`` file for that package. They'll be installed by opam.

Package Entry Page
------------------

The ``index.mld`` file (specified as ``index`` in ``mld_files``) is treated
specially by Dune. This will be the file used to generate the entry page for
the package, linked from the main package listing.

To generate pleasant documentation, we recommend writing an ``index.mld`` file
with at least short description of your package and possibly some examples.

If you do not write your own ``index.mld`` file, Dune will generate one with
the entry modules for your package. But this generated file will not be
installed.

.. _odoc-options:

Passing Options to ``odoc``
===========================

.. code-block:: dune

    (env
     (<profile>
      (odoc <optional-fields>)))

See :doc:`/reference/dune/env` for more details on the ``(env ...)``
stanza. ``<optional-fields>`` are:

- ``(warnings <mode>)`` specifies how warnings should be handled. ``<mode>``
  can be: ``fatal`` or ``nonfatal``. The default value is ``nonfatal``. This
  field is available since Dune 2.4.0 and requires odoc_ 1.5.0.

.. _odoc: https://github.com/ocaml-doc/odoc

Local Documentation Search Using Sherlodoc
==========================================

If Sherlodoc is installed, generated HTML documentation will include a
search bar. It supports search by name, documentation and fuzzy type search.

In can be installed with:

.. code:: console

  $ opam install sherlodoc
