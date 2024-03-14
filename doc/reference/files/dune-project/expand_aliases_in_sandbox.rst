expand_aliases_in_sandbox
-------------------------

.. describe:: (expand_aliases_in_sandbox ...)

   When a sandboxed action depends on an alias, copy the expansion of the alias
   inside the sandbox. For instance, in the following example:

   .. code:: dune

      (alias
       (name foo)
       (deps ../x))

      (cram
       (deps (alias foo)))

   File `x` will be visible inside the Cram test if and only if this option is
   enabled. This option is a better default in general; however, it currently
   causes Cram tests to run noticeably slower. So it is disabled by default
   until the performance issue with Cram test is fixed.
