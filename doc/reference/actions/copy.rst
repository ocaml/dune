copy
----

.. highlight:: dune

.. describe:: (copy <src> <dst>)

   Copy a file. If these files are OCaml sources, you should follow the
   ``module_name.xxx.ml`` :ref:`naming convention <merlin-filenames>` to
   preserve Merlin's functionality.

   Example::

   (copy data.txt.template data.txt)
