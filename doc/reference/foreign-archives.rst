Foreign Archives
----------------

Foreign archives are a way to link archives that are separately compiled.

They are particularly useful when embedding a library written in a foreign
language and/or built with another build system. See :ref:`foreign-sandboxing`
for more details.

To use this feature, use the ``foreign_archives`` field of the corresponding
``library`` or ``executable`` stanza. For example:

.. code:: dune

    (library
     (name lib)
     (foreign_stubs (language c) (names src1 src2))
     (foreign_stubs (language cxx) (names src3) (flags -O2))
     (foreign_archives arch1 some/dir/arch2))

Here, in addition to :doc:`foreign-stubs`, we also specify foreign archives
``arch1`` and ``arch2``, where the latter is stored in a subdirectory
``some/dir``.

You can build a foreign archive manually, e.g., using a custom ``rule`` as
described in :ref:`foreign-sandboxing`, or ask Dune to build it via the
``foreign_library`` stanza:

.. code:: dune

    (foreign_library
     (archive_name arch1)
     (language c)
     (enabled_if true)
     (names src4 src5)
     (extra_objects obj1)
     (include_dir headers))

This asks Dune to compile C source files ``src4`` and ``src5`` with
headers tracked in the ``headers`` directory and put the resulting
object files into an archive ``arch1``, whose full name is typically
``libarch1.a`` for static linking and ``dllarch1.so`` for dynamic
linking.

The ``foreign_library`` stanza supports all :doc:`foreign-stubs` fields.
The ``archive_name`` field specifies the archive's name. You can refer
to the same archive name from multiple OCaml libraries and executables, so a
foreign archive is a bit like a foreign library, hence the name of the stanza.
The ``enabled_if`` field has the same meaning as in the :doc:`dune/library`
stanza.
The ``extra_objects`` field specifies additional object files to be included.
Dune will look for ``obj1.o`` in this case. ``extra_objects`` uses the
:doc:`/reference/ordered-set-language` and supports ``(:include ...)`` forms.