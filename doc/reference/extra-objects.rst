Extra Objects
-------------

It's possible to specify native object files to be packaged with OCaml
libraries or linked into OCaml executables. Do this by using the
``extra_objects`` field of the ``library`` or ``executable`` stanzas.
For example:

.. code:: dune

    (executable
     (public_name main)
     (extra_objects foo bar))

    (rule
     (targets foo.o bar.o)
     (deps foo.c bar.c)
     (action (run ocamlopt %{deps})))

This example builds an executable which is linked against a pair of native
object files, ``foo.o`` and ``bar.o``. The ``extra_objects`` field takes a list
of object names, which correspond to the object file names with their path and
extension omitted (in contrast, ``(rule)`` manipulates file names, so the
extension needs to be provided).

In this example, the sources corresponding to the objects (``foo.c`` and
``bar.c``)  are assumed to be present in the same directory as the OCaml source
code, and a custom ``rule`` is used to compile the C source code into object
files using ``ocamlopt``. This is not necessary; one can instead compile foreign
object files manually and place them next to the OCaml source code.
