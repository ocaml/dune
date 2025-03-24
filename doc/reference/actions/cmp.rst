cmp
---

.. highlight:: dune

.. describe:: (cmp <file1> <file2>)

   ``(cmp <file1> <file2>)`` is similar to ``(run cmp <file1> <file2>)`` but
   allows promotion. See :doc:`/concepts/promotion` for more details.

   Example::

   (cmp bin.expected bin.output)
