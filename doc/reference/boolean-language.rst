##################
 Boolean Language
##################

The Boolean language allows the user to define simple Boolean
expressions that Dune can evaluate. Here's a semiformal specification of
the language:

.. productionlist:: blang op : '=' | '<' | '>' | '<>' | '>=' | '<=' expr : (and <expr>+) : (or <expr>+) : (<op> <template> <template>) : (not <expr>) : <template>

After an expression is evaluated, it must be exactly the string ``true``
or ``false`` to be considered as a Boolean. Any other value will be
treated as an error.

Below is a simple example of a condition expressing that the build has a
Flambda compiler, with the help of variable expansion, and is targeting
OSX:

.. code:: dune

   (and %{ocaml-config:flambda} (= %{ocaml-config:system} macosx))
