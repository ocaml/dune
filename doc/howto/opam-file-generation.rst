How to Generate Opam Files from ``dune-project``
================================================

.. highlight:: dune

This guide will show you how to configure Dune so that it generates opam files.

Declaring Package Dependencies
------------------------------

The goal of this first step is to add ``(package)`` stanzas in your
``dune-project`` file. These stanzas declare the metadata that your package
uses in the language of opam packages. See :ref:`declaring-a-package`.

The next step depends on whether you are starting from a clean slate (new
package) or adapting an existing opam file.

For a New Package (No Existing Opam File)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If your project does not have any opam files, you will have to find your
package dependencies. In the simple case, collect all the libraries
that appear in the ``(libraries)`` fields of your project and put this list in
the ``(depends)`` field of the corresponding ``(package)``. See
:doc:`../explanation/ocaml-ecosystem` for the difference between libraries and
packages.

Example: you have a library that looks like::

  (library
   (public_name frobnitz)
   (libraries lwt fmt))

You can declare the package as::

  (package
   (name frobnitz)
   (depends lwt fmt))

Also add common metadata using ``(authors)``, ``(maintainers)``, ``(license)``,
``(source)``, as well as a ``(synopsis)`` and a ``(description)`` for

For an Existing Package
^^^^^^^^^^^^^^^^^^^^^^^

If you already have an opam file (or several of them), you can convert it by
following the rules in :doc:`/reference/dune-project/package`.

For example, if your opam file looks like:

.. code:: opam

   opam-version: 2.0
   authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
   maintainer: ["team@mirage.org"]
   name: "cohttp-async"
   synopsis: "HTTP client and server for the Async library"
   description: "A _really_ long description"
   license: "ISC"
   bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
   homepage: "https://github.com/mirage/ocaml-cohttp/"
   dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
   build: [
     ["dune" "subst"] {dev}
     [
       "dune"
       "build"
       "-p"
       name
       "-j"
       jobs
       "@install"
       "@runtest" {with-test}
       "@doc" {with-doc}
     ]
   ]
   depends: [
     "dune" { >= "3.4" }
     "odoc" { with-doc }
     "cohttp" { >= "1.0.2" }
     "conduit-async" { >= "1.0.3" }
     "async" { >= "v0.10.0" }
   ]

You can express this as::

   (source (github mirage/ocaml-cohttp))
   (license ISC)
   (authors "Anil Madhavapeddy" "Rudi Grinberg")
   (maintainers "team@mirage.org")

   (package
    (name cohttp-async)
    (synopsis "HTTP client and server for the Async library")
    (description "A _really_ long description")
    (depends
     (cohttp (>= 1.0.2))
     (conduit-async (>= 1.0.3))
     (async (>= v0.10.0))))

General Notes and Tips
^^^^^^^^^^^^^^^^^^^^^^

- Do not declare a dependency on the ``dune`` and ``odoc`` packages. Dune will
  generate them with the right constraints.
- For fields that are common between packages (like ``(authors)`` or
  ``(license)``), you can use a global one rather than replicate it between
  packages.
- If you use a platform such as GitHub you can use ``(source)`` as a shorthand
  instead of specifying ``(bug_reports)``, ``(homepage)``, etc.
- ``(package)`` stanzas do not support all opam fields or complete syntax for
  dependency specifications. If the package you are adapting requires this,
  keep the corresponding opam fields in a ``pkg.opam.template`` file. See
  :doc:`../reference/packages`.
- It is not necessary to specify ``(version)``, this will be added at release
  time if you use `dune-release`_.

.. _dune-release: https://github.com/tarides/dune-release

Generating Opam Files
---------------------

If you have existing ``*.opam`` files, make a backup of them because the instructions in this section will overwrite them.

Now that you have declared package metadata in ``dune-project``, you can add
``(generate_opam_files)`` in ``(dune-project)``.

From now on, commands like ``dune build`` and ``dune runtest`` are going to regenerate the contents of opam files from the metadata in ``(package)`` stanzas.
If you only want to generate the opam file, run ``dune build <project_name>.opam``.

Run ``dune build`` once and observe that the opam files have been created or
updated. Make sure to add these changes to your version control system.
