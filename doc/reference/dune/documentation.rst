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

- ``(files <arg>)``: the ``files`` field accepts the same arguments as the one
  from the :ref:`install stanza <including-files-install-stanza>`. It allows to
  install ``mld`` files as well as asset files, and specify where they are in the
  hierarchy of documentation (with the ``as`` and ``with_prefix`` keyword). Note
  that dune supports installing those files, but not yet building the
  documentation with a non-flat hierarchy, or with non-mld files.

For more information, see :ref:`documentation`.
