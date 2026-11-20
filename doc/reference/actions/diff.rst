diff
----

.. highlight:: dune

.. describe:: (diff <file1> <file2>)

   ``(diff <file1> <file2>)`` is similar to ``(run diff <file1> <file2>)`` but
   is better and allows promotion. See :doc:`/concepts/promotion` for more
   details.

   Example::

   (diff test.expected test.output)

.. describe:: (diff? <file1> <file2>)

   Like ``diff``, but does nothing if ``<file2>`` is absent.

   .. versionchanged:: 3.26

      With :ref:`format-generated-corrections` set to ``true``, regular text
      corrections are formatted before comparison or promotion according to the
      formatting configuration for ``<file1>``.
