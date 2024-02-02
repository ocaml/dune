cache-check-probability
-----------------------

While the main purpose of Dune cache is to speed up build times, it can also be
used to check build reproducibility. It is possible to enable a probabilistic
check, in which Dune will re-execute randomly chosen build rules and compare
their results with those stored in the cache. If the results differ, the rule is
not reproducible, and Dune will print out a corresponding warning.

.. code:: dune

    (cache-check-probability <number>)

where ``<number>`` is a floating-point number between 0 and 1 (inclusive). 0
means never to check for reproducibility, and 1 means to always perform the
check.
