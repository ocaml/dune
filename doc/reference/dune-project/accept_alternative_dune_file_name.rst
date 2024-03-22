accept_alternative_dune_file_name
---------------------------------

.. describe:: (accept_alternative_dune_file_name ...)

   .. versionadded:: 3.0

   Specify that the alternative filename ``dune-file`` is accepted in addition
   to ``dune``.

   This may be useful to avoid problems with ``dune`` files that have the
   executable permission in a directory in the ``PATH``, which can unwittingly
   happen on Windows.

   Note that ``dune`` continues to be accepted even after enabling this option,
   but if a file named ``dune-file`` is found in a directory, it will take
   precedence over ``dune``.
