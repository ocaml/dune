######################
 terminal-persistence
######################

Specifies how Dune handles the terminal when a rebuild is triggered in
watch mode.

.. code:: dune

   (terminal-persistence <setting>)

where ``<setting>`` is one of:

-  ``preserve`` does not clear the terminal screen between rebuilds.
-  ``clear-on-rebuild`` clears the terminal screen between rebuilds.
-  ``clear-on-rebuild-and-flush-history`` clears the terminal between
   rebuilds, and it also deletes everything in the scrollback buffer.
