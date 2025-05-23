maintenance_intent
------------------

.. _maintenance_intent:
.. describe:: (maintenance_intent <strings>)

   .. versionadded:: 3.18

   Specify the `opam maintenance intent <https://github.com/ocaml/opam-repository/blob/master/governance/policies/archiving.md#specification-of-the-x--fields-used-in-the-archiving-process>`__.

   Valid for all packages defined in the current Dune project. May be overriden
   by the per-package field (see :doc:`package`).

   Example:

   .. code:: dune

      (maintenance_intent "(latest)")
