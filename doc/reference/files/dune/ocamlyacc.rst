ocamlyacc
---------

``(ocamlyacc <names>)`` is essentially a shorthand for:

.. code:: dune

    (rule
     (targets <name>.ml <name>.mli)
     (deps    <name>.mly)
     (action  (chdir %{workspace_root}
               (run %{bin:ocamlyacc} %{deps}))))

To use a different rule mode, use the long form:

.. code:: dune

    (ocamlyacc
     (modules <names>)
     (mode    <mode>))
