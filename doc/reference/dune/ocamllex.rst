ocamllex
--------

``(ocamllex <names>)`` is essentially a shorthand for:

.. code:: dune

    (rule
     (target <name>.ml)
     (deps   <name>.mll)
     (action (chdir %{workspace_root}
              (run %{bin:ocamllex} -q -o %{target} %{deps}))))

To use a different rule mode, use the long form:

.. code:: dune

    (ocamllex
     (modules <names>)
     (mode    <mode>))

Starting in Dune 3.25, the ``flags`` field passes additional command-line
arguments to ``ocamllex``. The flags use the
:doc:`/reference/ordered-set-language` and support
:doc:`/concepts/variables`. For example, ``-ml`` generates an OCaml-based
automaton instead of using the built-in automata interpreter:

.. code:: dune

    (ocamllex
     (modules lexer)
     (flags -ml))
