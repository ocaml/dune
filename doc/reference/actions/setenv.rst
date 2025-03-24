setenv
------

.. highlight:: dune

.. describe:: (setenv <var> <value> <DSL>)

   Run an action with an environment variable set.

   Example::

     (setenv
       VAR value
       (bash "echo $VAR"))
