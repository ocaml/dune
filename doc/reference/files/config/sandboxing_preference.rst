sandboxing_preference
---------------------

The preferred sandboxing setting. Individual rules may specify different
preferences. Dune will try to utilize a setting satisfying both conditions.

.. code:: dune

    (sandboxing_preference <setting> <setting> ...)

where each ``<setting>`` can be one of:

- ``none`` disables sandboxing.

- ``hardlink`` uses hard links for sandboxing. This is the default under Linux.

- ``copy`` copies files for sandboxing. This is the default under Windows.

- ``symlink`` uses symbolic links for sandboxing.
