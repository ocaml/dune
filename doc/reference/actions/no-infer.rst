no-infer
--------

.. highlight:: dune

.. describe:: (no-infer <DSL>)

   Perform an action without inference of dependencies and targets. This is
   useful if you are generating dependencies in a way that Dune doesn't know
   about, for instance by calling an external build system.

   Example::

     (no-infer
      (progn
       (run make)
       (copy mylib.a lib.a)))
