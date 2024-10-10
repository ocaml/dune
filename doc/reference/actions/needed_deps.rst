needed_deps
----

.. highlight:: dune

.. describe:: (needed_deps <file> ...)

   Declare the contents of ``<file>`` assumed to as dependencies of the rule.
   Each ``<file>`` is assumed to contain a valid dependency specification (see
   below).

   This is used to declare that certain "order only" dependencies (see
   :docs:`concepts/dependency-spec`) are actual dependencies of the rule.

   The file passed as an argument is assumed to contain zero or more
   S-expressions using the following constructors which cover a subset of the
   usual dependency sepecification:

   - ``(file <filename>)``
   - ``(alias <alias_name>)``
   - ``(file_selector <glob>)``
   - ``(universe)``

   (see :docs:`concepts/dependency-spec` for the details of each constructor).

   In the following example, both ``a`` and ``b`` are declared as "order only"
   dependencies. The file ``b`` is in addition declared as a needed dependency
   for the rule. The rule will only be reran if ``b`` changes, but not if ``a``
   changes.

   Example::

   (rule
    (deps (order_only (file a) (file b)))
    (action
     (progn
     (with-stdout-to deps (echo "(file b)"))
     (needed_deps deps))))
