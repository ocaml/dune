.. _blang:

****************
Boolean Language
****************

The boolean language allows the user to define simple boolean expressions that
dune can evaluate. Here's a semi formal specification of the language:

.. code::

   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   expr := (and <expr>+)
         | (or <expr>+)
         | (<op> <template> <template>)
         | <template>

After an expression is evaluated, it must be exactly the string ``true`` or
``false`` to be considered as a boolean. Any other value will be treated as an
error.

Here's a simple example of a condition that expresses running on OSX and having
an flambda compiler with the help of variable expansion:

.. code:: scheme

   (and %{ocamlc-config:flambda} (= %{ocamlc-config:system} macosx))
