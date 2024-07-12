``--only-packages`` - Restricting the Set of Packages
=====================================================

Restrict the set of packages from your workspace that Dune can see with
the ``--only-packages`` option:

.. code:: console

    $ dune build --only-packages pkg1,pkg2,... @install

This option acts as if you went through all the Dune files and
commented out the stanzas referring to a package that isn't in the list
given to ``dune``.
