######
 copy
######

.. highlight:: dune

.. dune:action:: copy
   :param: <src> <dst>

   Copy a file. If these files are OCaml sources, you should follow the
   ``module_name.xxx.ml`` :ref:`naming convention <merlin-filenames>` to
   preserve Merlin's functionality.

   Example:

   .. code::

      (copy data.txt.template data.txt)
