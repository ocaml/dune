pkg
====

.. warning::

   :doc:`Dune Package Management </explanation/package-management>` is not
   final yet and the configuration options are subject to change.

This stanza allows configuring the package management features of Dune.

.. describe:: (pkg <setting>)

    .. versionadded:: 3.20
    
    Where ``<setting>`` can be ``enabled``, ``disabled``, or, starting in Dune 3.22, ``autolocking``. The latter meaning the package management features will be enabled, but existing lock directories will be ignored.

Note that the command line option ``--pkg`` has precedence over this stanza.
