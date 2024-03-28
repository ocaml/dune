Using a Custom Code Generator
=============================

To generate a file ``foo.ml`` using a program from another directory:

.. code:: dune

    (rule
     (targets foo.ml)
     (deps    (:gen ../generator/gen.exe))
     (action  (run %{gen} -o %{targets})))
