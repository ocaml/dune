diff
----

.. highlight:: dune

.. describe:: (diff <file1> <file2>)

   ``(diff <file1> <file2>)`` is similar to ``(run diff <file1> <file2>)`` but
   is better and allows promotion. See :doc:`/concepts/promotion` for more
   details.

   Example::

   (diff test.expected test.output)
