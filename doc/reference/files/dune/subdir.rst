subdir
------

The ``subdir`` stanza can be used to evaluate stanzas in subdirectories. This is
useful for generated files or to override stanzas in vendored directories
without editing vendored ``dune`` files.

In this example, a ``bar`` target is created in the ``foo`` directory, and a bar
target will be created in ``a/b/bar``:

.. code:: dune

   (subdir foo (rule (with-stdout-to bar (echo baz))))
   (subdir a/b (rule (with-stdout-to bar (echo baz))))
