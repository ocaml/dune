*****************
API documentation
*****************

Jbuilder supports generating API documentation for libraries using the
`odoc tool <https://github.com/ocaml-doc/odoc>`__ in HTML format.

For this to work you need to have odoc installed and have
documentation comments in your OCaml source files following the syntax
described in the the section ``Text formatting`` of the `OCaml manual
<http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html>`_.

Generated pages
===============

Jbuilder stores the generated HTML pages in
``_build/<context>/_doc`. It creates one sub-directory per public
library and generates an ``index.html`` file in each sub-directory.

The documentation is never installed on the system by Jbuilder. It is
meant to be read locally while developping and then published on the
www when releasing packages.

Building the documentation
==========================

To build the documentaion, you can simply use the ``doc`` alias, which
depends on the generated HTML pages for all the public libraries.

For instance:

.. code:: bash

    $ jbuilder build @doc

Custom library indexes
======================

If the directory where a library lives contains a file named
``<lib-name>.mld``, Jbuilder will generate the library index from this
file. ``<lib-name>`` is what you put in the ``(name ...)`` field of the
library's jbuild file.

Such a file must contains text using the same syntax as ocamldoc
comments.
