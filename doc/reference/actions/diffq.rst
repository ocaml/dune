diff?
-----

.. highlight:: dune

.. describe:: (diff? <file1> <file2>)

   ``(diff? <file1> <file2>)`` is similar to ``(diff <file1> <file2>)`` except
   that ``<file2>`` should be produced by a part of the same action rather than
   be a dependency, is optional and will be consumed by ``diff?``.

   Example::

     (progn
      (with-stdout-to test.output (run ./test.exe))
      (diff? test.expected test.output))
