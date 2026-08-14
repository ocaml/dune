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

Starting in Dune 3.25, the ``flags`` field passes additional command-line
arguments to ``ocamlyacc``. The flags use the
:doc:`/reference/ordered-set-language` and support
:doc:`/concepts/variables`:

.. code:: dune

    (ocamlyacc
     (modules parser)
     (flags -q))
