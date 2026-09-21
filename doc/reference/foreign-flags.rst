Flags in Foreign Code
---------------------

Depending on the :doc:`dune-project/use_standard_c_and_cxx_flags` option,
the base ``:standard`` set of flags for C will contain only ``ocamlc_cflags`` or
both ``ocamlc_cflags`` and ``ocamlc_cppflags``.

When ``use_standard_c_and_cxx_flags`` is enabled, Dune adds
``-Werror=unguarded-availability-new`` to C and C++ ``:standard`` flags when
supported, in all profiles. Warnings in this group become errors, helping catch
calls to APIs unavailable on the deployment target. To opt out for particular
foreign stubs:

.. code:: dune

   (flags (:standard \ -Werror=unguarded-availability-new))

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

For example, to add a flag to all C stubs in the ``dev`` profile, write:

.. code:: dune

   (env
    (dev
     (c_flags (:standard -DMY_DEBUG_STUBS))))

To add flags only to one ``foreign_stubs`` field, write:

.. code:: dune

   (library
    (name mylib)
    (foreign_stubs
     (language c)
     (names mystubs)
     (flags (:standard -DMYLIB_STUBS))))

If the flags come from a generated file, for example from a
``dune-configurator`` script that queried ``pkg-config``, include them with
``(:include ...)``:

.. code:: dune

   (library
    (name mylib)
    (foreign_stubs
     (language c)
     (names mystubs)
     (flags (:standard (:include c_flags.sexp))))
    (c_library_flags (:standard (:include c_library_flags.sexp))))
