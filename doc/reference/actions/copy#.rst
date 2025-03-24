copy#
-----

.. highlight:: dune

.. describe:: (copy# <src> <dst>)

   Copy a file and add a line directive at the beginning.

   Example::

   (copy# config.windows.ml config.ml)

   More precisely, ``copy#`` inserts the following line:

   .. code:: ocaml

      # 1 "<source file name>"

   Most languages recognize such lines and update their current location to
   report errors in the original file rather than the copy. This is important
   because the copy exists only under the ``_build`` directory, and in order
   for editors to jump to errors when parsing the build system's output, errors
   must point to files that exist in the source tree. In the beta versions of
   Dune, ``copy#`` was called ``copy-and-add-line-directive``. However, most of
   time, one wants this behavior rather than a bare copy, so it was renamed to
   something shorter.
