jobs
----

Maximum number of concurrent jobs Dune is allowed to have.

.. code:: dune

    (jobs <setting>)

where ``<setting>`` is one of:

- ``auto``, auto-detect maximum number of cores. This is the default value.

- ``<number>``, a positive integer specifying the maximum number of jobs Dune
  may use simultaneously.
