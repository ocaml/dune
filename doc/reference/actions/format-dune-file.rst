cat
---

.. highlight:: dune

.. describe:: (format-dune-file <file>)

   Print the formatted contents of a file, assumed to contain S-expressions, to
   stdout. Note that the precise formatting can depend on the version of the
   Dune language used by containing project.

   Example::

   (format-dune-file file.sexp)
