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

.. grid:: 1 2 1 3

  .. grid-item::

    .. toctree::
      :caption: Components
      :maxdepth: 1
    
      executable
      library
      foreign_library
      deprecated_library_name
      generate_sites_module
      test
      cram
      toplevel
      documentation
      install
      plugin

  .. grid-item::
    
    .. toctree::
      :caption: Project structure
      :maxdepth: 1
    
      rule
      alias
      copy_files
      include
      dynamic_include
      env
      dirs
      data_only_dirs
      ignored_subdirs
      include_subdirs
      vendored_dirs
      subdir

  .. grid-item::

    .. toctree::
      :caption: Integrations
      :maxdepth: 1
    
      cinaps
      coq_theory
      mdx
      menhir
      ocamllex
      ocamlyacc
    
    .. toctree::
      :caption: Deprecated
      :maxdepth: 1
    
      jbuild_version
