ocamllex
--------

``(ocamllex <names>)`` is essentially a shorthand for:

.. code:: lisp

    (rule
     (target <name>.ml)
     (deps   <name>.mll)
     (action (chdir %{workspace_root}
              (run %{bin:ocamllex} -q -o %{target} %{deps}))))

To use a different rule mode, use the long form:

.. code:: scheme

    (ocamllex
     (modules <names>)
     (mode    <mode>))
