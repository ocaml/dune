documentation
-------------

Additional manual pages may be attached to packages using the ``documentation``
stanza. These ``.mld`` files must contain text in the same syntax as OCamldoc
comments.

.. code-block:: dune

  (documentation (<optional-fields>))

Where ``<optional-fields>`` are:

- ``(package <name>)`` defines the package this documentation should be attached
  to. If this is absent, Dune will try to infer it based on the location of the
  stanza.

- ``(mld_files <arg>)``: the ``<arg>`` field follows the
  :doc:`/reference/ordered-set-language`. This is a set of extensionless MLD file
  basenames attached to the package, where ``:standard`` refers to all the
  ``.mld`` files in the stanza's directory.

For more information, see :ref:`documentation`.
