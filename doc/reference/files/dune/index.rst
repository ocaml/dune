.. _dune-files:

dune
====

``dune`` files are the main part of Dune. They are used to describe libraries,
executables, tests, and everything Dune needs to know about.

The syntax of ``dune`` files is described in
:doc:`/reference/lexical-conventions`.

``dune`` files are composed of stanzas, as shown below:

.. code:: dune

    (library
     (name mylib)
     (libraries base lwt))

    (rule
     (target foo.ml)
     (deps   generator/gen.exe)
     (action (run %{deps} -o %{target})))

The following pages describe the available stanzas and their meanings.

.. include:: ../../../stanzas/alias.rst
.. include:: ../../../stanzas/cinaps.rst
.. include:: ../../../stanzas/copy_files.rst
.. include:: ../../../stanzas/coq_theory.rst
.. include:: ../../../stanzas/cram.rst
.. include:: ../../../stanzas/data_only_dirs.rst
.. include:: ../../../stanzas/deprecated_library_name.rst
.. include:: ../../../stanzas/dirs.rst
.. include:: ../../../stanzas/documentation.rst
.. include:: ../../../stanzas/env.rst
.. include:: ../../../stanzas/executable.rst
.. include:: ../../../stanzas/external_variant.rst
.. include:: ../../../stanzas/foreign_library.rst
.. include:: ../../../stanzas/generate_sites_module.rst
.. include:: ../../../stanzas/ignored_subdirs.rst
.. include:: ../../../stanzas/include.rst
.. include:: ../../../stanzas/include_subdirs.rst
.. include:: ../../../stanzas/install.rst
.. include:: ../../../stanzas/jbuild_version.rst
.. include:: ../../../stanzas/library.rst
.. include:: ../../../stanzas/mdx.rst
.. include:: ../../../stanzas/menhir.rst
.. include:: ../../../stanzas/ocamllex.rst
.. include:: ../../../stanzas/ocamlyacc.rst
.. include:: ../../../stanzas/plugin.rst
.. include:: ../../../stanzas/rule.rst
.. include:: ../../../stanzas/subdir.rst
.. include:: ../../../stanzas/test.rst
.. include:: ../../../stanzas/toplevel.rst
.. include:: ../../../stanzas/vendored_dirs.rst
.. include:: ../../../stanzas/dynamic_include.rst
