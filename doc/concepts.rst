****************
General Concepts
****************

.. _diffing-and-promotion:

Diffing and Promotion
=====================

``(diff <file1> <file2>)`` is very similar to ``(run diff <file1>
<file2>)``. In particular it behaves in the same way:

- When ``<file1>`` and ``<file2>`` are equal, it does nothing.
- When they are not, the differences are shown and the action fails.

However, it is different for the following reason:

- The exact command used for diff files can be configured via the
  ``--diff-command`` command line argument. Note that it's only
  called when the files are not byte equals

- By default, it will use ``patdiff`` if it is installed. ``patdiff``
  is a better diffing program. You can install it via opam with:

  .. code:: sh

     $ opam install patdiff

- On Windows, both ``(diff a b)`` and ``(diff? a b)`` normalize
  end-of-line characters before comparing the files.

- Since ``(diff a b)`` is a built-in action, Dune knows that ``a``
  and ``b`` are needed, so you don't need to specify them
  explicitly as dependencies.

- You can use ``(diff? a b)`` after a command that might or might not
  produce ``b``, for cases where commands optionally produce a
  *corrected* file

- If ``<file1>`` doesn't exist, it will compare with the empty file.

- It allows promotion. See below.

Note that ``(cmp a b)`` does no end-of-line normalization and doesn't
print a diff when the files differ. ``cmp`` is meant to be used with
binary files.

Promotion
---------

Whenever an action ``(diff <file1> <file2>)`` or ``(diff?  <file1>
<file2>)`` fails because the two files are different, Dune allows
you to promote ``<file2>`` as ``<file1>`` if ``<file1>`` is a source
file and ``<file2>`` is a generated file.

More precisely, let's consider the following Dune file:

.. code:: dune

   (rule
    (with-stdout-to data.out (run ./test.exe)))

   (rule
    (alias   runtest)
    (action (diff data.expected data.out)))

Where ``data.expected`` is a file committed in the source
repository. You can use the following workflow to update your test:

- Update the code of your test.
- Run ``dune runtest``. The diff action will fail and a diff will
  be printed.
- Check the diff to make sure it's what you expect. This diff can be displayed
  again by running ``dune promotion diff``.
- Run ``dune promote``. This will copy the generated ``data.out``
  file to ``data.expected`` directly in the source tree.

You can also use ``dune runtest --auto-promote``, which will
automatically do the promotion.

Package Specification
=====================

Installation is the process of copying freshly built libraries,
binaries, and other files from the build directory to the system. Dune
offers two ways of doing this: via opam or directly via the ``install``
command. In particular, the installation model implemented by Dune
was copied from opam. Opam is the standard OCaml package manager.

In both cases, Dune only know how to install whole packages. A
package being a collection of executables, libraries, and other files.
In this section, we'll describe how to define a package, how to
"attach" various elements to it, and how to proceed with installing it
on the system.

.. _declaring-a-package:

Declaring a Package
-------------------

To declare a package, simply add a ``package`` stanza to your
``dune-project`` file:

.. code:: dune

          (package
           (name mypackage)
           (synopsis "My first Dune package!")
           (description "\| This is my first attempt at creating
                        "\| a project with Dune.
          ))

Once you have done this, Dune will know about the package named
``mypackage`` and you will be able to attach various elements to it.
The ``package`` stanza accepts more fields, such as dependencies.

Note that package names are in a global namespace, so the name you choose must
be universally unique. In particular, package managers never allow to
release two packages with the same name.

.. TODO: describe this more in details

In older projects using Dune, packages were defined by manually writing a file
called ``<package-name>.opam`` at the root of the project. However, it's not
recommended to use this method in new projects, as we expect to deprecate it in
the future. The right way to define a package is with a ``package`` stanza in
the ``dune-project`` file.

See :ref:`opam-generation` for instructions on configuring Dune to automatically
generate ``.opam`` files based on the ``package`` stanzas.

Attaching Elements to a Package
-------------------------------

Attaching an element to a package means declaring to Dune that this
element is part of the said package. The method to attach an element
to a package depends on the kind of the element. In this sub-section,
we will go through the various kinds of elements and describe how to
attach each of them to a package.

In the rest of this section, ``<prefix>`` refers to the directory in
which the user chooses to install packages. When installing via opam,
it's opam that sets this directory. When calling ``dune install``,
the installation directory is either guessed or can be manually
specified by the user. Defaults directories which replace guessing
can be set during the compilation of dune.

Sites of a Package
------------------

When packages need additional resources outside their binary, their location
could be hard to find. Moreover some packages could add resources to another
package, for example in the case of plugins. These location are called sites in
Dune. One package can define them. During execution, one site corresponds to a
list of directories. They are like layers, and the first directories have a higher
priority. Examples and precisions are available at :ref:`sites`.


Libraries
^^^^^^^^^

In order to attach a library to a package, merely add a
``public_name`` field to your library. This is the name that external
users of your libraries must use in order to refer to it. Dune
requires that the public name of a library is either the name of the
package it is part of or start with the package name followed by a dot
character.

For instance:

.. code:: dune

   (library
    (name mylib)
    (public_name mypackage.mylib))

After you have added a public name to a library, Dune will know to
install it as part of the package it is attached to. Dune installs
the library files in a directory ``<prefix>/lib/<package-name>``.

If the library name contains dots, the full directory in which the
library files are installed is ``lib/<comp1>/<comp2/.../<compn>``,
where ``<comp1>``, ``<comp2>``, ... ``<compn>`` are the dot separated
component of the public library name. By definition, ``<comp1>`` is
always the package name.

Executables
^^^^^^^^^^^

Similarly to libraries, to attach an executable to a package simply
add a ``public_name`` field to your ``executable`` stanza or a
``public_names`` field for ``executables`` stanzas. Designate this
name to match the available executables through the installed ``PATH``
(i.e., the name users must type in their shell to execute
the program), because Dune cannot guess an executable's relevant package
from its public name. It's also necessary to add a ``package`` field
unless the project contains a single package, in which case the executable
will be attached to this package.

For instance:

.. code:: dune

          (executable
           (name main)
           (public_name myprog)
           (package mypackage))

Once ``mypackage`` is installed on the system, the user will be able
to type the following in their shell:

::

   $ myprog

to execute the program.

Other Files
^^^^^^^^^^^

For all other kinds of elements, you must attach them manually via
an :ref:`install` stanza.


.. _foreign-sources-and-archives:

Foreign Sources, Archives and Objects
=====================================

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
  ``cxx`` means C++. In future, more languages may be supported.
- ``names`` specifies the *names* of source files. When specifying a source
  file, omit the extension and any relative parts of the path;
  Dune will scan all library directories to find all matching files and
  raise an error if multiple source files map to the same object name.
  If you need to have multiple object files with the same name, you can
  package them into different :ref:`foreign-archives` via the
  ``foreign_archives`` field. This field uses the
  :doc:`concepts/ordered-set-language` where the ``:standard`` value
  corresponds to the set of names of all source files whose extensions match
  the specified ``language``.
- ``flags`` are passed when compiling source files. This field is specified
  using the :doc:`concepts/ordered-set-language`, where the ``:standard`` value
  comes from the environment settings ``c_flags`` and ``cxx_flags``,
  respectively. Note that, for C stubs, Dune unconditionally adds the flags
  present in the OCaml config fields ``ocamlc_cflags`` and ``ocamlc_cppflags``
  to the compiler command line. This behavior can be disabled since Dune 2.8
  via the ``dune-project`` option :ref:`always-add-cflags`.
- ``include_dirs`` are tracked as dependencies and passed to the compiler
  via the ``-I`` flag. You can use :doc:`concepts/variables` in this field and
  refer to a library source directory using the ``(lib library-name)`` syntax.
  Additionally, the syntax ``(include filename)`` can be used to specify a file
  containing additional arguments to ``(include_dirs ...)``. The named file can
  either contain a single path to be added to this list of include directories,
  or an S-expression listing additional ``(include_dirs ...)`` arguments (the
  ``(lib ...)`` and ``(include ...)`` syntax is also supported in files included
  in this way).
  For example, ``(include_dirs dir1 (lib lib1) (lib lib2) (include inc1) dir2)`` specifies
  the directory ``dir1``, the source directories of ``lib1`` and ``lib2``,
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
     (names src4 src5)
     (include_dir headers))

This asks Dune to compile C source files ``src4`` and ``src5`` with
headers tracked in the ``headers`` directory and put the resulting
object files into an archive ``arch1``, whose full name is typically
``libarch1.a`` for static linking and ``dllarch1.so`` for dynamic
linking.

The ``foreign_library`` stanza supports all :ref:`foreign-stubs` fields plus
the ``archive_name`` field, which specifies the archive's name. You can refer
to the same archive name from multiple OCaml libraries and executables, so a
foreign archive is a bit like a foreign library, hence the name of the stanza.

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

Depending on the :ref:`always-add-cflags` option, the base `:standard` set of
flags for C will contain only ``ocamlc_cflags`` or both ``ocamlc_cflags`` and
``ocamlc_cppflags``.

There are multiple levels where one can declare custom flags (using the
:doc:`concepts/ordered-set-language`), and each level inherits the flags of the
previous one in its `:standard` set:

- In the global `env` definition of a `dune-workspace` file
- In the per-context `env` definitions in a `dune-workspace` file
- In the env definition of a `dune` file
- In a `foreign_` field of an executable or a library

The ``%{cc}`` :doc:`variable <concepts/variables>` will contain the flags from
the first three levels only.
