cache-storage-mode
------------------

Specify the mechanism used by the Dune cache for storage.

.. code:: dune

    (cache-storage-mode <setting>)

where ``<setting>`` is one of:

- ``auto`` lets Dune decide the best mechanism to use.

- ``hardlink`` uses hard links for entries in the cache. If the cache is stored
  in a different partition than the one where the build is taking place, then
  this mode will not work and ``copy`` should be used instead.

- ``copy`` copies entries to the cache. This is less efficient than using hard
  links.
