Flags in Foreign Code
---------------------

Depending on the :doc:`dune-project/use_standard_c_and_cxx_flags` option,
the base ``:standard`` set of flags for C will contain only ``ocamlc_cflags`` or
both ``ocamlc_cflags`` and ``ocamlc_cppflags``.

There are multiple levels where one can declare custom flags (using the
:doc:`ordered-set-language`), and each level inherits the flags of the previous
one in its `:standard` set:

- In the global :doc:`dune-workspace/env` definition of a
  :doc:`dune-workspace/index` file
- In the :doc:`per-context env <dune-workspace/context>` definitions in a
  :doc:`dune-workspace/index` file
- In the :doc:`dune/env` definition of a :doc:`dune/index` file
- In a :doc:`foreign_stubs <foreign-stubs>`, :doc:`foreign_library
  <foreign-archives>` or :doc:`extra_objects <extra-objects>` field of an
  executable or a library.

The ``%{cc}`` :doc:`variable <../concepts/variables>` will contain the flags
from the first three levels only.
