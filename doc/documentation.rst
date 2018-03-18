************************
Generating Documentation
************************

Documentation in jbuilder is done courtesy of the odoc_ tool. Therefore, to
generate documentation in jbuilder, you will need to install this tool. This
should likely be done with opam:

::

  $ opam install odoc

Building Documentation
======================

Building the documentation using the ``@doc`` alias. Hence, all that is required
to generate documentation for your project is building this alias:

::

  $ jbuilder build @doc

An index page containing links to all the opam packages in your project can be
found in:

::

  $ open _build/default/_doc/_html/index.html

Documentation for private libraries may also be built with:

::

  $ jbuilder build @doc-private

But this libraries will not be in the main html listing above, since they do not
belong to any particular package. But the generated html will still be found in
``_build/default/_doc/_html/<library>``.

Attaching Documentation
=======================

Attaching documentation to packages can be done using the ``documentation`` stanza.

.. code-block:: lisp

  (documentation (<optional-fields>)))


Where ``<optional-fields>`` are:

- ``(package <name>)`` the package this documentation should be attached to. If
  this absent, jbuilder will try to infer it based on the location of the
  stanza.

- ``(mld_files <arg>)`` where ``<arg>`` field follows the
  :ref:`ordered-set-language`. This is a set of extension-less, mld file base
  names that are attached to the package. Where ``:standard`` refers to all the
  ``.mld`` files in the stanza's directory.

The ``index.mld`` file (specified as ``index`` in ``mld_files``) is treated
specially by odoc. This will be the file used to generate the entry page for the
package.

All mld files attached to a package will be included in the generated
``.install`` file for that package, and hence will be installed by opam.

Examples
--------

This stanza use attach all the .mld files in the current directory in a project
with a single package.

.. code-block:: lisp

   (documentation ())

This stanza will attach three mld files to package foo. The ``mld`` files should
be named ``foo.mld``, ``bar.mld``, and ``baz.mld``

.. code-block:: lisp

   (documentation
    ((package foo)
     (mld_files (foo bar baz))))

This stanza will attach all mld files excluding ``wip.mld`` in the current
directory to the inferred package:

.. code-block:: lisp

   (documentation
    ((mld_files (:standard \ wip))))

.. _odoc: https://github.com/ocaml-doc/odoc
