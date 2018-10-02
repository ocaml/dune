******************************
Dealing with foreign libraries
******************************

The OCaml programming language allows to interface libraries written
in foreign languages such as C. This section explains how to do this
with Dune. Note that it does not cover how to write the C stubs
themselves, this is covered by the OCaml manual:
https://caml.inria.fr/pub/docs/manual-ocaml/intfc.html.

More precisely, this section covers:
- how to add C/C++ stubs to an OCaml library
- how to pass specific compilaction flags for compiling the stubs
- how to build a library with a foreign build system

Note that in general Dune has limited support for building source
files written in foreign languages. This support is suitable for most
OCaml projects containing C stubs, but is too limited for building
complex libraries written in C or other languages. For such cases,
Dune allows to integrate a foreign build system into a normal Dune
build.

Adding C/C++ stubs to an OCaml library
======================================

To add C stubs to an OCaml library, simply list the C files without
the ``.c`` extension via the ``c_names`` field of the :ref:`library`
stanza. For instance:

.. code:: scheme

          (library
           (name mylib)
           (c_names file1 file2))

Similarly, you can add C++ stubs to an OCaml library by listing them
without the ``.cpp`` extension via the ``cxx_names`` field.

Dune is currently not flexible regarding the extension of the C/C++
source files. They have to be ``.c`` and ``.cpp``. If you have source
files that that do not follow this extension and you want to build
them with Dune, you need to rename them first. Alternatively, you can
use the :ref:`foreign build sandboxing <foreign-sandboxing>` method
described bellow.

Header files
------------

C/C++ source files may include header files in the same directory as
the C/C++ source files or in the same directory group when using
:ref:`include_subdirs`.

The header files must have the ``.h`` extension.

Installing header files
-----------------------

It is sometimes desirable to install header files with the
library. For that you have two choices: install them explicitely with
an :ref:`install` stanza or use the ``install_c_headers`` field of the
:ref:`library` stanza. This field takes a list of header files names
without the ``.h`` extension. When a library install header files,
these are made visible to users of the library via the include search
path.

