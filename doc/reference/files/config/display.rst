#########
 display
#########

Specify the amount of Dune’s verbosity.

.. code:: dune

   (display <setting>)

where ``<setting>`` is one of:

-  ``progress``, Dune shows and updates a status line as build goals are
   being completed. This is the default value.
-  ``verbose`` prints the full command lines of programs being executed
   by Dune, with some colors to help differentiate programs.
-  ``short`` prints a line for each program executed with the binary
   name on the left and the targets of the action on the right.
-  ``quiet`` only display errors.
