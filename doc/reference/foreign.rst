Foreign Sources, Archives, and Objects
======================================

Dune provides basic support for including foreign source files as well
as archives of foreign object files into OCaml projects via the
``foreign_stubs`` and ``foreign_archives`` fields. Individual object
files can also be included via the ``extra_objects`` field.

.. _foreign-stubs:

Foreign Stubs
-------------

You can specify foreign sources using the ``foreign_stubs`` field of the
``library`` and ``executable`` stanzas. For example:

.. code:: dune

    (library
     (name lib)
     (foreign_stubs (language c) (names src1 src2))
     (foreign_stubs (language cxx) (names src3) (flags -O2)))

Here we declare an OCaml library ``lib``, which contains two C sources
``src1`` and ``src2``, and one C++ source, ``src3``, which need to be
compiled with ``-O2``. These source files will be compiled and packaged
with the library, along with the link-time flags to be used when
linking the final executables. When matching ``names`` to source files,
Dune treats ``*.c`` files as C sources, and ``*.cpp``, ``*.cc``, and
``*.cxx`` files as C++ sources.

Here is a complete list of supported subfields:

- ``language`` specifies the source language, where ``c`` means C and
  ``cxx`` means C++. In the future, more languages may be supported.
- ``names`` specifies the *names* of source files. When specifying a source
  file, omit the extension and any relative parts of the path;
  Dune will scan all library directories to find all matching files and
  raise an error if multiple source files map to the same object name.
  If you need to have multiple object files with the same name, you can
  package them into different :ref:`foreign-archives` via the
  ``foreign_archives`` field. This field uses the :doc:`ordered-set-language`
  where the ``:standard`` value corresponds to the set of names of all source
  files whose extensions match the specified ``language``.
- ``flags`` are passed when compiling source files. This field is specified
  using the :doc:`ordered-set-language`, where the ``:standard`` value comes
  from the environment settings ``c_flags`` and ``cxx_flags``, respectively.
  Note that, for C stubs, Dune unconditionally adds the flags present in the
  OCaml config fields ``ocamlc_cflags`` and ``ocamlc_cppflags`` to the compiler
  command line. This behavior can be disabled since Dune 2.8 via the
  ``dune-project`` option :doc:`dune-project/use_standard_c_and_cxx_flags`.
- ``include_dirs`` are tracked as dependencies and passed to the compiler
  via the ``-I`` flag. You can use :doc:`../concepts/variables` in this field
  and refer to a library source directory using the ``(lib library-name)``
  syntax.
  Additionally, the syntax ``(include filename)`` can be used to specify a file
  containing additional arguments to ``(include_dirs ...)``. The named file can
  either contain a single path to be added to this list of include directories,
  or an S-expression listing additional ``(include_dirs ...)`` arguments (the
  ``(lib ...)`` and ``(include ...)`` syntax is also supported in files included
  in this way).
  For example, ``(include_dirs dir1 (lib lib1) (lib lib2) (include inc1) dir2)`` specifies
  the directory ``dir1``, the source directories of ``lib1``, and ``lib2``,
  the list of directories contained in the file ``inc1``,
  and the directory ``dir2``, in this order.
  Some examples of possible contents of the file ``inc1`` are:

  - ``dir3`` which would add ``dir3`` to the list of include directories
  - ``((lib lib3) dir4 (include inc2))`` which would add the source directory of
    the library ``lib3``, the directory ``dir4``, and the result of recursively
    including the contents of the file ``inc2``.
    The contents of included directories are tracked recursively, e.g., if you
    use ``(include_dir dir)`` and have headers ``dir/base.h`` and
    ``dir/lib/lib.h``, they both will be tracked as dependencies.
  - ``extra_deps`` specifies any other dependencies that should be tracked.
    This is useful when dealing with ``#include`` statements that escape into
    a parent directory like ``#include "../a.h"``.


Mode-Dependent Stubs
^^^^^^^^^^^^^^^^^^^^

Since Dune 3.5, it is possible to use different foreign stubs when building in
`native` or `byte` mode. This feature needs to be activated by adding ``(using
mode_specific_stubs 0.1)`` in the ``dune-project`` file.

Then it is allowed to use the ``mode`` field when describing ``foreign_stubs``.
If the same stub is defined twice, Dune will automatically chose the correct one.
This allows the use of different sets of flags or even different source files
from which the stubs are built.

.. code:: dune

  (executable
   (name main)
   (modes native byte_complete)
   (foreign_stubs
     (language c)
     (mode byte)
     (names c_stubs))
   (foreign_stubs
     (language c)
     (mode native)
     (flags :standard -DNATIVE_CODE) ; A flag specific to native builds
     (names c_stubs)))  ; This could be the name of an implementation
                        ; specific to native builds

Note that, as of version ``0.1`` of this extension, this mechanism does not work for ``foreign_archives``.

.. _foreign-archives:

Foreign Archives
----------------

You can also specify archives of separately compiled foreign object files
that need to be packaged with an OCaml library or linked into an OCaml
executable. To do that, use the ``foreign_archives`` field of the
corresponding ``library`` or ``executable`` stanza. For example:

.. code:: dune

    (library
     (name lib)
     (foreign_stubs (language c) (names src1 src2))
     (foreign_stubs (language cxx) (names src3) (flags -O2))
     (foreign_archives arch1 some/dir/arch2))

Here, in addition to :ref:`foreign-stubs`, we also specify foreign archives
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
     (include_dir headers))

This asks Dune to compile C source files ``src4`` and ``src5`` with
headers tracked in the ``headers`` directory and put the resulting
object files into an archive ``arch1``, whose full name is typically
``libarch1.a`` for static linking and ``dllarch1.so`` for dynamic
linking.

The ``foreign_library`` stanza supports all :ref:`foreign-stubs` fields.
The ``archive_name`` field specifies the archive's name. You can refer
to the same archive name from multiple OCaml libraries and executables, so a
foreign archive is a bit like a foreign library, hence the name of the stanza.
The ``enabled_if`` field has the same meaning as in the :doc:`dune/library`
stanza.

Foreign archives are particularly useful when embedding a library written in
a foreign language and/or built with another build system. See
:ref:`foreign-sandboxing` for more details.


.. _extra-objects:

Extra Objects
-------------

It's possible to specify native object files to be packaged with OCaml
libraries or linked into OCaml executables. Do this by using the
``extra_objects`` field of the ``library`` or ``executable`` stanzas.
For example:

.. code:: dune

    (executable
     (public_name main)
     (extra_objects foo bar))

    (rule
     (targets foo.o bar.o)
     (deps foo.c bar.c)
     (action (run ocamlopt %{deps})))

This example builds an executable which is linked against a pair of native
object files, ``foo.o`` and ``bar.o``. The ``extra_objects`` field takes a
list of object names, which correspond to the object file names with their path
and extension omitted.

In this example, the sources corresponding to the objects (``foo.c`` and
``bar.c``)  are assumed to be present in the same directory as the OCaml source
code, and a custom ``rule`` is used to compile the C source code into object
files using ``ocamlopt``. This is not necessary; one can instead compile foreign
object files manually and place them next to the OCaml source code.

.. _flags-flow:

Flags
-----

Depending on the :doc:`dune-project/use_standard_c_and_cxx_flags` option,
the base `:standard` set of flags for C will contain only ``ocamlc_cflags`` or
both ``ocamlc_cflags`` and ``ocamlc_cppflags``.

There are multiple levels where one can declare custom flags (using the
:doc:`ordered-set-language`), and each level inherits the flags of the previous
one in its `:standard` set:

- In the global ``env`` definition of a ``dune-workspace`` file
- In the per-context `env` definitions in a `dune-workspace` file
- In the env definition of a `dune` file
- In a `foreign_` field of an executable or a library

The ``%{cc}`` :doc:`variable <../concepts/variables>` will contain the flags
from the first three levels only.
