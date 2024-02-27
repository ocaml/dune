######
 dune
######

``dune`` files are the main part of Dune. They are used to describe
libraries, executables, tests, and everything Dune needs to know about.

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

.. toctree::

   alias
   cinaps
   copy_files
   coq_theory
   cram
   data_only_dirs
   deprecated_library_name
   dirs
   documentation
   dynamic_include
   env
   executable
   external_variant
   foreign_library
   generate_sites_module
   ignored_subdirs
   include
   include_subdirs
   install
   jbuild_version
   library
   mdx
   menhir
   ocamllex
   ocamlyacc
   plugin
   rule
   subdir
   test
   toplevel
   vendored_dirs
