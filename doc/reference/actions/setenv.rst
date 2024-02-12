setenv
------

.. highlight:: dune

.. dune:action:: setenv
   :param: <var> <value> <DSL>

   Run an action with an environment variable set.

   Example::

     (setenv
       VAR value
       (bash "echo $VAR"))
