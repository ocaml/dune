subst
-----

.. describe:: (subst <bool>)

   .. versionadded:: 3.0

   Control whether :ref:`dune-subst` is enabled for this project.

   - ``(subst disabled)``, means that any call of ``dune subst`` in this
     project is forbidden and will result in an error. This line will be
     omitted from the build instructions when generating opam files.
   - ``(subst enabled)`` allows substitutions explicitly. This is the default.
