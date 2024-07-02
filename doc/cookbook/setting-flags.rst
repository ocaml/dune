Setting the OCaml Compilation Flags Globally
============================================

``dune``
  .. code:: dune
  
      (env
       (dev
        (flags (:standard -w +42)))
       (release
        (ocamlopt_flags (:standard -O3))))

``dev`` and ``release`` correspond to build profiles. The build profile
can be selected from the command line with ``--profile foo`` or from a
:doc:`/reference/files/dune-workspace/index` file by writing:

.. code:: dune

    (profile foo)
