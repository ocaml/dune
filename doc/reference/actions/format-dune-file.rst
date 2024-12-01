cat
---

.. highlight:: dune

.. describe:: (format-dune-file <src> <dst>)

   Output the formatted contents of the file ``<src>`` to ``<dst>``. The source
   file is assumed to contain S-expressions. Note that the precise formatting
   can depend on the version of the Dune language used by containing project.

   Example::

   (format-dune-file file.sexp file.sexp.formatted)
