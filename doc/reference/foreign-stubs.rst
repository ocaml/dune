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
  package them into different :doc:`foreign-archives` via the
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
- ``extra_deps`` specifies any other dependencies that should be tracked.  This
    is useful when dealing with ``#include`` statements that escape into a
    parent directory like ``#include "../a.h"``.


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
