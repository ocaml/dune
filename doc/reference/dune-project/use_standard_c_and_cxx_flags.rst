use_standard_c_and_cxx_flags
----------------------------

.. describe:: (use_standard_c_and_cxx_flags ...)

   .. versionadded:: 2.8

   Control how flags coming from ``ocamlc -config`` are passed to the C
   compiler command line.

   Historically, they have been systematically prepended without a way to
   override them.

   If the following is passed, the mechanism is slightly altered:

   .. code:: dune

       (use_standard_c_and_cxx_flags)

   In this mode, Dune will populate the ``:standard`` set of C flags with the
   content of ``ocamlc_cflags`` and  ``ocamlc_cppflags``. These flags can be
   completed or overridden using the :doc:`/reference/ordered-set-language`.

   This is the default in the language version 3.0.
