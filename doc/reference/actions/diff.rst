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

      For text diffs, regular correction files are formatted before comparison
      or promotion, according to the formatting configuration for ``<file1>``.
      This supports OCamlFormat and Dune's built-in formatter for ``dune`` and
      ``dune-project``. See :doc:`../dune-project/formatting`.
