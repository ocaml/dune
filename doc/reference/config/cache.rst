cache
-----

Specifies whether Dune is allowed to store and fetch build targets from the Dune
cache.

.. code:: dune

    (cache <setting>)

where ``<setting>`` is one of:

- ``enabled`` enables Dune cache.

- ``exclude-user-rules`` enables Dune cache, but exclude user-written
  rules. This setting is a conservative choice that can avoid breaking rules
  whose dependencies are not correctly specified. Currently the default.

- ``disabled`` disables Dune cache.
